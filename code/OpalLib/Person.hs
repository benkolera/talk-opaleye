{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpalLib.Person where

--import Control.Arrow                   (returnA)
import Control.Lens                    (makeLenses)--,(^.))
import Data.Text                       (Text)
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Opaleye
import Opaleye.Classy

import OpalLib.Ids

data Person' a b = Person
  { _personId   :: a
  , _personName :: b
  } deriving Show
makeLenses ''Person'

makeAdaptorAndInstance "pPerson" ''Person'

type PersonColumns = Person' PersonIdColumn (Column PGText)
type PersonInsertColumns = Person' PersonIdColumnMaybe (Column PGText)
type Person = Person' PersonId Text

personTable :: Table PersonInsertColumns PersonColumns
personTable = Table "person" $ pPerson Person
  { _personId   = pPersonId . PersonId $ optional "id"
  , _personName = required "name"
  }

personQuery :: Query PersonColumns
personQuery = queryTable personTable

peopleAll :: CanOpaleye c e m => m [Person]
peopleAll = liftQuery personQuery
