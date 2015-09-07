{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens  ((^.),_1)
import Data.Time     (getCurrentTime,utctDay,UTCTime(..),secondsToDiffTime,addDays)
import Opaleye       (constant,pgStrictText)

import OpalLib.Accession
import OpalLib.Book
import OpalLib.Ids
import OpalLib.Loan
import OpalLib.Pagination
import OpalLib.Search
import OpalLib.Util

main :: IO ()
main = do
  eg_allBooks
  eg_findLyah
  eg_allProgrammingBooks
  eg_allProgrammingAccessions
  eg_borrow
  eg_return
  eg_overdue
  eg_bookCount
  eg_accessionCountKeywords
  eg_pagination
  eg_search
  eg_searchPaginated

lyahIsbn :: Isbn
lyahIsbn = Isbn 9781593272838

eg_allBooks :: IO ()
eg_allBooks = opaleyeExample "All Books" bookQuery booksAll

eg_findLyah :: IO ()
eg_findLyah = opaleyeExample "Find by ISBN"
  (findBookByIsbnQ (constant lyahIsbn))
  (findBookByIsbn lyahIsbn)

eg_allProgrammingBooks :: IO ()
eg_allProgrammingBooks = opaleyeExample "All Programming Books"
  (booksWithKeywordQuery (pgStrictText "Programming"))
  (booksWithKeyword "Programming")

eg_allProgrammingAccessions :: IO ()
eg_allProgrammingAccessions = opaleyeExample "All Programming Accessions"
  (accessionsWithKeywordQuery (pgStrictText "Programming"))
  (accessionsWithKeyword "Programming")

eg_borrow :: IO ()
eg_borrow = do
  printTitle "Borrow Book (Insert Loan)"
  b <- getCurrentTime
  let d = UTCTime (addDays 28 $ utctDay b) (secondsToDiffTime (60*60*17))
  printOpaleye $ do
    lId <- borrow (AccessionId 1) (PersonId 1) b d
    findLoanByLoanId lId
  printEnding

eg_return :: IO ()
eg_return = do
  printTitle "Return Book (Update Loan)"
  r  <- getCurrentTime
  ls <- runOpaleye loansOutstanding
  case ls of
    Right (fl:_) -> printOpaleye $ do
      let lId = fl^._1.loanId
      loanReturn lId r
      findLoanByLoanId lId
    _ -> putStrLn "ERROR: NO OUTSTANDING LOANS"
  printEnding

eg_overdue :: IO ()
eg_overdue = opaleyeExample "Overdue Books" loansOverdueQuery loansOverdue

eg_bookCount :: IO ()
eg_bookCount = opaleyeExample "Book Count"
  (accessionCountForBookQuery $ constant lyahIsbn)
  (accessionCountForBook lyahIsbn)

eg_accessionCountKeywords :: IO ()
eg_accessionCountKeywords = opaleyeExample "Accession Keyword Grouping"
  accessionCountsForKeywordQuery
  accessionCountsForKeyword

-- NOTE: The query in the example output 
eg_pagination :: IO ()
eg_pagination = do
  printTitle "Paginated Programming Books"
  printOpaleye $ booksWithKeywordPaginated (Pagination 1 5) "Programming"
  printOpaleye $ booksWithKeywordPaginated (Pagination 2 5) "Programming"
  printOpaleye $ booksWithKeywordPaginated (Pagination 3 5) "Programming"
  printEnding

eg_search :: IO ()
eg_search = do
  let s1 = Search (Just "Great Good") [] False
  opaleyeExample "Search Title" (searchQuery s1) (search s1)
  let s2 = Search Nothing ["Management"] False
  opaleyeExample "Search Keyword" (searchQuery s2) (search s2)
  let s3 = Search Nothing ["Management"] True
  opaleyeExample "Search Keyword" (searchQuery s3) (search s3)

eg_searchPaginated :: IO ()
eg_searchPaginated = do
  let s1 = Search Nothing ["Programming"] False
  printTitle "Search Programming Paginated"
  printOpaleye $ searchPaginated (Pagination 1 5) s1
  printOpaleye $ searchPaginated (Pagination 2 5) s1
  printOpaleye $ searchPaginated (Pagination 3 5) s1
  printEnding
