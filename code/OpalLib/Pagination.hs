{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module OpalLib.Pagination where

import Control.Applicative ((<$>),(<*>))
import Control.Lens   (makeLenses)
import Data.Int       (Int64)
import Data.Profunctor.Product.Default (Default)
import Opaleye        (Query,QueryRunner,PGInt8,Column,limit,offset,aggregate,count)
import Opaleye.Classy (CanOpaleye,liftQuery,liftQueryFirst)

data Pagination = Pagination
  { _paginationPage  :: Int
  , _paginationWidth :: Int
  }
makeLenses ''Pagination

data PaginationResults a = PaginationResults
  { _paginationResultsPage    :: Int
  , _paginationResultsWidth   :: Int
  , _paginationResultsMaxPage :: Int64
  , _paginationResultsRows    :: [a]
  } deriving (Show,Functor)
makeLenses ''PaginationResults

paginateQuery :: Pagination -> Query a -> Query a
paginateQuery (Pagination p w) = limit w . offset ((p-1) * w)

countQuery :: (a -> Column i) -> Query a -> Query (Column PGInt8)
countQuery getId = aggregate count . fmap getId

paginationResults :: Pagination -> Maybe Int64 -> [a] -> PaginationResults a
paginationResults (Pagination pp pw) cMay res = PaginationResults pp pw pm res
  where
    pm = maybe 0 pageCount cMay
    pageCount :: Int64 -> Int64
    pageCount c = ceiling (fromIntegral c / fromIntegral pw :: Double)

paginate
  :: (CanOpaleye c e m, Default QueryRunner a b)
  => Pagination
  -> (a -> Column i)
  -> Query a
  -> m (PaginationResults b)
paginate p getId q = paginationResults p
  <$> liftQueryFirst (countQuery getId q)
  <*> liftQuery      (paginateQuery p q)
