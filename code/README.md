# OpalLib - Demo Opaleye Code & Tutorial

## Installation & Running Instructions

You don't need to run the code to follow along, but if you want to see things
work / inspect types / tinker then you may want to set things up.

```
psql postgres -c "CREATE DATABASE opallib" && psql opallib -f setup.sql

cabal sandbox init
cabal install --only-dependencies
```

Before running the code, you'll need to edit the connectInfo hardcoded in:

[OpalLib.Util](OpalLib/Util.hs)

To run all the queries, print the Queries and the output:
```
cabal run 
```

To get a repl that you can inspect types and run things by hand:
```
cabal repl
```

## Why Opaleye

Opaleye is an embedded DSL written in Haskell that generates SQL and runs it
against a postgres database. It focuses on:

- Fine grained composability of query elements
- Being true to SQL and offering all of SQLs awesome features (joins,
aggregations, etc)
- While maintaining a typesafe API that will not generate SQL that fails at
runtime.

This is an awesome thing, because so many applications talk to databases and it
has always been saddening to get hit with the same old tradeoffs / problems:

- Raw SQL is easy but not safely composable
- Some things restrict what you can do so you lose valuable parts of SQL
(joins,aggregations, etc)
- Some things need your data types constrained so much that it's impractical to
  use the library without giving up a lot of control to the libraries template
  haskell functions.
- The API can still generate SQL that fails at runtime.
- Or even if it can do all of that, the density of the API and type errors make
  you angry and go back to one of the less-safe-but-easier options.

Opaleye achieves all of its goals without any of these tradeoffs, which (I
think) is super exciting and worth your time to check it out! :smile:

## Table Definition

### Record Definition

First we define a datatype with a hole record accessor and a type parameter for
each column. This seems crazy at first, but it makes a lot of sense later!

```
data Book' a b = Book
  { _bookIsbn  :: a
  , _bookTitle :: b
  } deriving Show
makeLenses ''Book'
```

We then make shorthand types for the database types (Columns) and the types that
we want to end up with once we run our query.

```
type BookColumns = Book' IsbnColumn (Column PGText)
type Book = Book' Isbn Text
```

See [http://hackage.haskell.org/package/opaleye-0.4.1.0/docs/Opaleye-PGTypes.html](Opaleye.PGTypes)
for the full list of column types.

We then line these columns up to a table and column names with a table definition.

```
makeAdaptorAndInstance "pBook" ''Book'

bookTable :: Table BookColumns BookColumns
bookTable = Table "book" $ pBook Book
  { _bookIsbn  = pIsbn . Isbn $ required "isbn"
  , _bookTitle = required "title"
  }
```

[OpalLib/Book.hs](See in full in Book.hs)

### Isomorphic to a Tuple

### Product Profunctor

### Newtypes

### Default Columns / Serial IDs

## Querying

### QueryTable

### Columns

### Query

### Running Queries

### Restriction

### Arrows

### Join

### Projection

### Reusing Joins

## Inserts / Updates

### constant

### insert

### update

## Aggregation

### count

### aggregate / Aggregator

### groupBy

### other Aggregate Functions

### pagination

## The Big Finale

### search

### paginated search

## Room For Improvement

### Dates

### Newtypes

### Varchar Lengths

### Prefetching

### Like anchoring / escaping

## Wrap Up

Core concepts:

- Table: Sources the table name, column names, insert column types and read
column types.
- Column: An expression inside an SQL query (Literal,ColumnName,Compound
Expression)
- Query o: A query that we can either join to another query, restrict or run on
the database.
to do its bit.
- ProductProfunctor: Magic sauce for transforming each column in a record or
  tuple. Depending on the profunctor, lots of different things can be done
  (UnpackSpec,Constant,Aggregation,etc.).

## Acknowledgements & Further Resources

Thanks to [https://github.com/tomjaguarpaw/](Tom Ellis) for the lovely library
and all the hard work that others have done on HaskellDB (a precursor to
Opaleye).

Hackage: http://hackage.haskell.org/package/opaleye

Github:  https://github.com/tomjaguarpaw/haskell-opaleye
