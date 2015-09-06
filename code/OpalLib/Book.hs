{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpalLib.Book where

import Control.Arrow                   (returnA)
import Control.Lens                    (makeLenses,(^.),to)
import Data.Text                       (Text)
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)  
import Opaleye
import Opaleye.Classy

import OpalLib.Ids
import OpalLib.BookKeyword

data Book' a b = Book
  { _bookIsbn  :: a
  , _bookTitle :: b
  } deriving Show
makeLenses ''Book'

makeAdaptorAndInstance "pBook" ''Book'

type BookColumns = Book' IsbnColumn (Column PGText)
type Book = Book' Isbn Text

bookTable :: Table BookColumns BookColumns
bookTable = Table "book" $ pBook Book
  { _bookIsbn  = pIsbn . Isbn $ required "isbn"
  , _bookTitle = required "title"
  }

bookQuery :: Query BookColumns
bookQuery = queryTable bookTable

booksAll :: CanOpaleye c e m => m [Book]
booksAll = liftQuery bookQuery

findBookByIsbnQ :: IsbnColumn -> Query BookColumns
findBookByIsbnQ isbn = proc () -> do
   b <- bookQuery -< ()
   restrict -< unIsbn (b^.bookIsbn) .== unIsbn isbn
   returnA -< b

findBookByIsbn :: CanOpaleye c e m => Isbn -> m (Maybe Book)
findBookByIsbn = liftQueryFirst . findBookByIsbnQ . constant

booksWithKeywordQuery :: Column PGText -> Query BookColumns
booksWithKeywordQuery kw = proc () -> do
  b <- bookQuery -< ()
  bookRestrictedByKeyword kw -< b
  returnA -< b

bookRestrictedByKeyword :: Column PGText -> QueryArr BookColumns ()
bookRestrictedByKeyword kw = proc (b) -> do
  k <- bookKeywordQuery -< ()
  restrict -< b^.bookIsbn.to unIsbn .== k^.bookKeywordBookIsbn.to unIsbn
  restrict -< k^.bookKeywordKeyword .== kw

booksWithKeyword :: CanOpaleye c e m => Text -> m [Book]
booksWithKeyword = liftQuery . booksWithKeywordQuery . constant
