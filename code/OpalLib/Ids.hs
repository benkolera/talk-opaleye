{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OpalLib.Ids where

import Data.Int                        (Int64)
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)  
import Opaleye                         (PGInt8,PGInt4,Column,Nullable)

-- Accession ID ----------------------------------------------------------

data AccessionId' a = AccessionId { unAccessionId :: a } deriving Show
makeAdaptorAndInstance "pAccessionId" ''AccessionId'

type AccessionId = AccessionId' Int
type AccessionIdColumn = AccessionId' (Column PGInt4)
type AccessionIdColumnMaybe = AccessionId' (Maybe (Column PGInt4))
type AccessionIdColumnNullable = AccessionId' (Column (Nullable PGInt4))

-- ISBN ------------------------------------------------------------------

data Isbn' a = Isbn { unIsbn :: a } deriving Show
makeAdaptorAndInstance "pIsbn" ''Isbn'

type IsbnColumn = Isbn' (Column PGInt8)
type Isbn       = Isbn' Int64

-- Loan ID ---------------------------------------------------------------

data LoanId' a = LoanId { unLoanId :: a } deriving Show
makeAdaptorAndInstance "pLoanId" ''LoanId'

type LoanId = LoanId' Int
type LoanIdColumn = LoanId' (Column PGInt4)
type LoanIdColumnMaybe = LoanId' (Maybe (Column PGInt4))
type LoanIdColumnNullable = LoanId' (Column (Nullable PGInt4))

-- Person ID -------------------------------------------------------------

data PersonId' a = PersonId { unPersonId :: a } deriving Show
makeAdaptorAndInstance "pPersonId" ''PersonId'

type PersonId = PersonId' Int
type PersonIdColumn = PersonId' (Column PGInt4)
type PersonIdColumnMaybe = PersonId' (Maybe (Column PGInt4))
type PersonIdColumnNullable = PersonId' (Column (Nullable PGInt4))
