{-# LANGUAGE Arrows                #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpalLib.BookKeyword where

--import Control.Arrow                   (returnA)
import Control.Lens                    (makeLenses)--,(^.))
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)  
import Data.Text                       (Text)
import Opaleye

import OpalLib.Ids

data BookKeyword' a b = BookKeyword
  { _bookKeywordBookIsbn :: a
  , _bookKeywordKeyword  :: b
  } deriving Show
makeLenses ''BookKeyword'

makeAdaptorAndInstance "pBookKeyword" ''BookKeyword'

type BookKeywordColumns = BookKeyword' IsbnColumn (Column PGText)
type BookKeyword = BookKeyword' Isbn Text

bookKeywordTable :: Table BookKeywordColumns BookKeywordColumns
bookKeywordTable = Table "book_keyword" $ pBookKeyword BookKeyword
  { _bookKeywordBookIsbn = pIsbn . Isbn $ required "book_isbn"
  , _bookKeywordKeyword  = required "keyword"
  }

bookKeywordQuery :: Query BookKeywordColumns
bookKeywordQuery = queryTable bookKeywordTable
