{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpalLib.Accession where

import Control.Arrow                   (returnA)
import Control.Lens                    (makeLenses,(^.),to,_2)
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Data.Text                       (Text)
import Opaleye
import Opaleye.Classy

import OpalLib.Ids
import OpalLib.Book

data Accession' a b = Accession
  { _accessionId       :: a
  , _accessionBookIsbn :: b
  } deriving Show
makeLenses ''Accession'

makeAdaptorAndInstance "pAccession" ''Accession'

type AccessionColumns = Accession' AccessionIdColumn IsbnColumn
type AccessionInsertColumns = Accession'
  AccessionIdColumnMaybe
  IsbnColumn
type Accession = Accession' AccessionId Isbn 

accessionTable :: Table AccessionInsertColumns AccessionColumns
accessionTable = Table "accession" $ pAccession Accession
  { _accessionId       = pAccessionId . AccessionId $ optional "id"
  , _accessionBookIsbn = pIsbn . Isbn $ required "book_isbn"
  }

accessionQuery :: Query AccessionColumns
accessionQuery = queryTable accessionTable

accessionsAll :: CanOpaleye c e m => m [Accession]
accessionsAll = liftQuery accessionQuery

accessionsWithBookQuery :: Query (AccessionIdColumn,BookColumns)
accessionsWithBookQuery = proc () -> do
  a <- accessionQuery -< ()
  b <- bookQuery      -< ()
  restrict -< a^.accessionBookIsbn.to unIsbn .== b^.bookIsbn.to unIsbn
  returnA -< (a^.accessionId,b)

accessionsWithKeywordQuery
  :: Column PGText
  -> Query (AccessionIdColumn,BookColumns)
accessionsWithKeywordQuery kw = proc () -> do
  t <- accessionsWithBookQuery -< ()
  bookRestrictedByKeyword kw -< t^._2
  returnA -< t

accessionsWithKeyword :: CanOpaleye c e m => Text -> m [(AccessionId,Book)]
accessionsWithKeyword = liftQuery . accessionsWithKeywordQuery . constant
