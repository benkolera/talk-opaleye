{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
module OpalLib.Date where

import           Opaleye (Column,PGTimestamptz)
import qualified Opaleye.Internal.Column              as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

now :: Column PGTimestamptz
now = C.Column (HPQ.FunExpr "now" [])
