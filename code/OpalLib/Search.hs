{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module OpalLib.Search where

import Control.Arrow              (returnA)
import Control.Applicative        ((<$>))
import Control.Lens               ((^.),to,makeLenses)
import Data.Bool                  (bool)
import Data.List.NonEmpty         (nonEmpty)
import Data.Text                  (Text)
import Data.Profunctor.Product    (p2)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)  
import Data.Semigroup             ((<>))
import Data.Time                  (UTCTime)
import Opaleye
import Opaleye.Classy

import OpalLib.Loan
import OpalLib.Accession
import OpalLib.Book
import OpalLib.Ids
import OpalLib.Pagination

data Search = Search
  { _searchTitle         :: Maybe Text
  , _searchKeywords      :: [Text]
  , _searchAvailableOnly :: Bool
  }
makeLenses ''Search

data SearchResult' a b c d = SearchResult
  { _searchResultAccessionId :: a
  , _searchResultBook        :: b
  , _searchResultKeywords    :: c
  , _searchResultDueBack     :: d
  } deriving Show
makeLenses ''SearchResult'

type SearchResultColumns = SearchResult'
  AccessionIdColumn
  BookColumns
  (Column (PGArray PGText))
  (Column (Nullable PGTimestamptz))

type SearchResult = SearchResult'
  AccessionId
  Book
  [Text]
  (Maybe UTCTime)

makeAdaptorAndInstance "pSearchResult" ''SearchResult'

accessionWithDueDateQuery :: Query (AccessionIdColumn,BookColumns,Column (Nullable PGTimestamptz))
accessionWithDueDateQuery = fmap project
  . leftJoin accessionsWithBookQuery loanQuery
  $ \ ((a,_),l) ->
    unAccessionId a .== l^.loanAccessionId.to unAccessionId 
    .&& loanOutstanding l
  where
    project :: ((AccessionIdColumn,BookColumns),LoanColumnsNullable)
            -> (AccessionIdColumn,BookColumns,Column (Nullable PGTimestamptz))
    project = (\ ((a,b),l) -> (a,b,l^.loanDue))

searchAccessionIdQuery
  :: Search
  -> Query (AccessionIdColumn,Column (PGArray PGText))
searchAccessionIdQuery s = aggregate agg $ proc () -> do
  (a,b,l) <- accessionWithDueDateQuery -< ()
  k <- bookKeywordJoin -< b
  restrictMaybe likeTitle (s^.searchTitle) -< b
  restrict -< bool (constant True) (isNull l) (s^.searchAvailableOnly)
  restrictMaybe (\ k kws -> in_ (constant <$> kws) k) (nonEmpty $ s^.searchKeywords) -< k
  returnA -< (a,k)
  where
    agg = p2 (pAccessionId $ AccessionId groupBy, arrayAgg)
    likeTitle b t = (b^.bookTitle) `like` (constant $ "%" <> t <> "%")

searchQuery :: Search -> Query SearchResultColumns
searchQuery s = proc () -> do
  (a,b,l)   <- accessionWithDueDateQuery -< ()
  (aId,kws) <- searchAccessionIdQuery s -< ()
  restrict  -< unAccessionId aId .== unAccessionId a
  returnA   -< SearchResult a b kws l

search :: CanOpaleye c e m => Search -> m [SearchResult]
search = liftQuery . searchQuery

searchPaginated
  :: CanOpaleye c e m
  => Pagination
  -> Search
  -> m (PaginationResults SearchResult)
searchPaginated p s = paginate p
  (^.searchResultAccessionId.to unAccessionId)
  (searchQuery s)

restrictMaybe :: (c -> a -> Column PGBool) -> Maybe a -> QueryArr c ()
restrictMaybe f m = proc (c) -> do
  restrict -< maybe (constant True) (f c) m
