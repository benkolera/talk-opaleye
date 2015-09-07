{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpalLib.Accession where

import Control.Arrow                   (returnA)
import Control.Lens                    (makeLenses,(^.),to,_1,_2)
import Data.Int                        (Int64)
import Data.Profunctor.Product         (p2)
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Data.Maybe                      (fromMaybe)
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
  -> Query (AccessionIdColumn,BookColumns,Column PGText)
accessionsWithKeywordQuery kw = proc () -> do
  t <- accessionsWithBookQuery -< ()
  k <- bookRestrictedByKeyword kw -< t^._2
  returnA -< (t^._1,t^._2,k)

accessionsWithKeyword
  :: CanOpaleye c e m
  => Text
  -> m [(AccessionId,Book,Text)]
accessionsWithKeyword = liftQuery . accessionsWithKeywordQuery . constant

accessionsForBookQuery :: IsbnColumn -> Query (AccessionIdColumn,BookColumns)
accessionsForBookQuery isbn = proc () -> do
  t <- accessionsWithBookQuery -< ()
  restrict -< t^._2.bookIsbn.to unIsbn .== unIsbn isbn
  returnA -< t

accessionCountForBookQuery :: IsbnColumn -> Query (Column PGInt8)
accessionCountForBookQuery =
  aggregate count . fmap (^._1.to unAccessionId) . accessionsForBookQuery

accessionCountForBook :: CanOpaleye c e m => Isbn -> m Int64
accessionCountForBook =
  fmap (fromMaybe 0) . liftQueryFirst . accessionCountForBookQuery . constant

accessionCountsForKeywordQuery :: Query (Column PGText,Column PGInt8)
accessionCountsForKeywordQuery =
  orderBy (desc (^._2)) . aggregate (p2 (groupBy,count)) $ proc () -> do
    (aId,b) <- accessionsWithBookQuery -< ()
    k       <- bookKeywordJoin -< b
    returnA -< (k,aId^.to unAccessionId)

accessionCountsForKeyword :: CanOpaleye c e m => m [(Text,Int64)]
accessionCountsForKeyword = liftQuery accessionCountsForKeywordQuery
