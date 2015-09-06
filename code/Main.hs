{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens  ((^.),_1)
import Control.Monad (when)
import Data.Time     (getCurrentTime)
import Opaleye       (constant,pgStrictText)

import OpalLib.Accession
import OpalLib.Book
import OpalLib.Ids
import OpalLib.Loan
import OpalLib.Person
import OpalLib.Util

testAll :: Bool
testAll = False

main :: IO ()
main = do
  eg_allBooks
  eg_findLyah
  eg_allProgrammingBooks
  eg_allProgrammingAccessions
  eg_borrow
  eg_return

  when testAll $ do
    eg_allAccessions
    eg_allPeople
    eg_allLoans
    eg_allProgrammingBooks

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
  printOpaleye $ do
    lId <- borrow (AccessionId 1) (PersonId 1) b
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

eg_allAccessions :: IO ()
eg_allAccessions = opaleyeExample "All Accessions"
  accessionQuery
  accessionsAll

eg_allPeople :: IO ()
eg_allPeople = opaleyeExample "All People" personQuery peopleAll

eg_allLoans :: IO ()
eg_allLoans = opaleyeExample "All Loans" loanQuery loansAll
