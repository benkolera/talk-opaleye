{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
module Noc.Db.Date where

import           Opaleye (Column,PGDate,PGTimestamp,PGTimestamptz)
import qualified Opaleye.Internal.Column              as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

class PGHasTimestamp a
instance PGHasTimestamp PGDate
instance PGHasTimestamp PGTimestamp
instance PGHasTimestamp PGTimestamptz

class PGHasDate a where
instance PGHasDate PGDate
instance PGHasDate PGTimestamp
instance PGHasDate PGTimestamptz

now :: Column PGTimestamptz
now = C.Column (HPQ.FunExpr "now" [])

date :: PGHasDate a => Column a -> Column PGDate
date a = C.Column (HPQ.FunExpr "date" [C.unColumn a])

timestamp :: PGHasTimestamp a => Column a -> Column PGTimestamp
timestamp a = C.Column (HPQ.CastExpr "timestamp" (C.unColumn a))

timestamptz :: PGHasTimestamp a => Column a -> Column PGTimestamptz
timestamptz a = C.Column (HPQ.CastExpr "timestamptz" (C.unColumn a))
