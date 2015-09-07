# OpalLib - Demo Opaleye Code & Tutorial

## Installation & Running Instructions

You don't need to run the code to follow along, but if you want to see things
work / inspect types / tinker then you may want to set things up.

```sh
psql postgres -c "CREATE DATABASE opallib" && psql opallib -f setup.sql

cabal sandbox init
cabal install --only-dependencies
```

Before running the code, you'll need to edit the connectInfo hardcoded in:

[OpalLib.Util](OpalLib/Util.hs)

To run all the queries, print the Queries and the output:
```sh
cabal run 
```

To get a repl that you can inspect types and run things by hand:
```sh
cabal repl
```

And if you want to follow on with the database schema that we are working with,
take a peek at the [DB setup script](setup.sql).

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
- Some DB APIs restrict what you can do so you lose valuable parts of SQL
(joins,aggregations, etc)
- Some APIs need your data types constrained so much that it's impractical to
  use the library without giving up a lot of control to the libraries template
  haskell functions.
- You give up control over performance and can get very slow (albeit correct) queries.
- The API can still generate SQL that fails at runtime.
- Or even if it can do all of that, the density of the API and type errors make
  you angry and go back to one of the less-safe-but-easier options.

Opaleye comes achieves the goal of composable, typesafe SQL without any of these
problems save for a moderate (but not impassable) learning curve.

It is super refreshing to be able to treat your SQL with the same dose of safe
abstraction as the rest of you haskell code, and for that alone it's well worth
diving in and trying it out for yourself! :smile:

## Table Definition

First we need to tell Opaleye what our tables and columns are. These table
definitions form the starting point for our queries.

### Record Definition

First we define a datatype with a hole record accessor and a type parameter for
each column. This seems crazy at first, but it makes a lot of sense later!

```haskell
data Book' a b = Book
  { _bookIsbn  :: a
  , _bookTitle :: b
  } deriving Show
makeLenses ''Book'
```

We then make shorthand types for the database types (Columns) and the types that
we want to end up with once we run our query.

```haskell
type BookColumns = Book' IsbnColumn (Column PGText)
type Book = Book' Isbn Text
```

See [Opaleye.PGTypes](http://hackage.haskell.org/package/opaleye-0.4.1.0/docs/Opaleye-PGTypes.html)
for the full list of column types.

We then line these columns up to a table and column names with a table definition.

```haskell
makeAdaptorAndInstance "pBook" ''Book'

bookTable :: Table BookColumns BookColumns
bookTable = Table "book" $ pBook Book
  { _bookIsbn  = pIsbn . Isbn $ required "isbn"
  , _bookTitle = required "title"
  }
```

See [OpalLib.Book](OpalLib/Book.hs) for the full code.

### Isomorphic to a Tuple

Note that this is the same thing as having a tuple except that we have names for
each hole rather than 1,2,3,4,5.

```haskell
type Book' a b = (a,b)
type BookColumns = (IsbnColumn,(Column PGText))
type Book = (Isbn,Text)
```

Except that [Data.Profunctor.Product](http://hackage.haskell.org/package/product-profunctors-0.6.3/docs/Data-Profunctor-Product.html)
already has a product profunctor for tuples from 1-26 widths!

```haskell
bookTable :: Table BookColums BookColumns
bookTable = Table "book" $ p2
  ( pIsbn . Isbn $ required "isbn"
  , required "title"
  )
```

This becomes really useful once we start joining and projecting on other columns
as we can always use a tuple instead of making a type just for that set of
return columns. It's really up to you and whether you're dealing with that set
of columns enough to give them proper names (the names help!).

### Product Profunctor

But what are these weird pBook / p2 things?

```haskell
λ> :t pBook
pBook
  :: Data.Profunctor.Product.ProductProfunctor p =>
     Book' (p a1_0 a1_1) (p a2_0 a2_1)
     -> p (Book' a1_0 a2_0) (Book' a1_1 a2_1)
```

The easiest way to think about profunctors is to specialise them to (->).

```haskell
λ> :t pBook (Book (* 2) (++"bar"))
pBook (Book (* 2) (++"bar"))
  :: Book' Int [Char] -> Book' Int [Char]
λ> pBook (Book (* 2) (++"bar")) (Book 2 "foo")
Book {_bookIsbn = 4, _bookTitle = "foobar"}
```

In essence, the product profunctor gives us the ability to run transformations
on each column. Depending on the profunctor, we get a different transformation.
There are a lot of different profunctors in use in Opaleye to reduce
boilerplate:

- TableDefiniton: For constructing table definitions (you saw this just before!) 
- Constant: for constructing a column literal from a haskell value
- QueryRunner: For going from a column to a haskell value when a query is run.
- Aggregate: Used for constructing aggregations on a set of columns.
- Etc.

We'll see more on these later! Don't worry, it'll start making more sense soon!

### Newtypes

It would be counterproductive to our goals to just record the primary key of the
table (in Book, this was ISBN) just as a PGInt8 as that would mean that when we
are joining tables we could easily mess up and join the wrong PGInt8s together.

```haskell
data Isbn' a = Isbn { unIsbn :: a } deriving Show
makeAdaptorAndInstance "pIsbn" ''Isbn'

type IsbnColumn = Isbn' (Column PGInt8)
type Isbn       = Isbn' Int64
```

The annoying thing about this is that whenever you want to deal with the column
there is some extra unwrapping in places that you don't actually want it. Like
what we saw in the table definition:

```haskell
bookTable = Table "book" $ p2
  ( pIsbn . Isbn $ required "isbn"
  , required "title"
  )
```

And it's also annoying once you start have the column be autoincrementing or
nullable, but the extra safety when joining is definititely worth the
clunkiness. 

See these in full in [OpalLib.Ids](OpalLib/Ids.hs)

### Default Columns / Serial IDs

When we saw the table definition, remember the weird thing where our table
definition had two type parameters. We didn't use it for Books, but the first
type parameter is for noting that some of the columns for insertion are optional
because the database will default the value (sequence based id, etc.).

```haskell
type PersonColumns = Person' PersonIdColumn (Column PGText)
type PersonInsertColumns = Person' PersonIdColumnMaybe (Column PGText)
type Person = Person' PersonId Text

personTable :: Table PersonInsertColumns PersonColumns
personTable = Table "person" $ pPerson Person
  { _personId   = pPersonId . PersonId $ optional "id"
  , _personName = required "name"
  }
```

This means that later on when we insert we can pass in Nothing for the ID and
have the database assign it for us.

See this in full in [OpalLib.Person](OpalLib/Person.hs)

## Querying

Now that we have defined our tables, we need to start doing what we came here
for: making SQL!

### QueryTable & Columns

The queryTable takes a table definition and essentially creates a ```SELECT * FROM```
foo query" that you can build other queries from.

```haskell
bookQuery :: Query BookColumns
bookQuery = queryTable bookTable
```

That query has all of the column names as the return type, so we have access to
all of those columns to restrict and project.

The query generated looks like this:

```sql
SELECT "isbn0_1" as "result1_2",
       "title1_1" as "result2_2"
FROM (SELECT *
      FROM (SELECT "isbn" as "isbn0_1",
                   "title" as "title1_1"
            FROM "book" as "T1") as "T1") as "T1"
```

Which is probably not how you'd write it, but with the wonders of the Postgres
Query planner should be more or less the same performance as the idealised SQL.
There are plans to improve the query generator over time so that it can get
closer to the ideal SQL that you'd expect, but correctness is an overruling
goal. Tom makes the claim that if your query is being generated in a way that is
significantly slower than the ideal SQL, that is considered a bug and he'll do
what he can to optimise that case. 

### Columns & Queries

So we have two main building blocks for Opaleye.

- Columns: An individual SQL expression used in restrictions or projections. This comes in three main forms:
  - Column Reference: the columns that our output from queryTable
  - Literals: A literal value in the SQL (e.g. ```pgStrictText "foo" :: Column
    PGText``` which corresponds to a 'foo' in the actual SQL)
  - Compound expressions: E.g. ```(b^.bookTitle .== pgStrictText "foo") :: Column PGBool```
- Queries: A fully runnable SQL query. Can be joined to other queries.

### Running Queries

Opaleye contains many different ways to run queries, which are defined in:

[Opaleye.RunQuery](http://hackage.haskell.org/package/opaleye-0.4.1.0/docs/Opaleye-RunQuery.html)

The main one that we want is:

```haskell
runQuery :: Default QueryRunner columns haskells => Connection -> Query columns -> IO [haskells]
```

Which looks scary, but reads:

> Give me a DB connection and a Query of columns, and so long as there is a way to
  convert columns to the haskell values, I'll run the query against the database
  and give you back the haskell values.

You guessed it, QueryRunner is a ProductProfunctor, so as long as you've got a
transformation between each column and the haskell type then it'll all just work.

Lets see it in action:

```haskell
type IsbnColumn = Isbn' (Column PGInt8)
type BookColumns = Book' IsbnColumn (Column PGText)

type Isbn       = Isbn' Int64
type Book = Book' Isbn Text

booksAll :: Connection -> IO Book
booksAll c = runQuery c (queryTable bookTable)
```

This works because there is a ```QueryRunnerColumnDefault PGInt8 Int64``` and
```QueryRunnerColumnDefault PGText Text``` and the makeAdaptorAndInstance
defined a productprofunctor instance for Book.

To see which QueryRunnerColumnDefault instances there are, take a look at this
list of instances here:

http://hackage.haskell.org/package/opaleye-0.4.1.0/docs/Opaleye-Internal-RunQuery.html#t:QueryRunnerColumnDefault

Because of this type magic, it is always required to have type signatures on
your functions that are actually running the query and returning haskell values.
You'll see this all over OpalLib.

Also, in OpalLib the running of the query is slightly different:

```haskell
booksAll :: CanOpaleye c e m => m [Book]
booksAll = liftQuery bookQuery
```

Which uses the monad transformer versions of the RunQuery functions. These are
defined in a library that I wrote called
[Opaleye.Classy](http://hackage.haskell.org/package/opaleye-classy). Use of this
is completely optional but I find it easier than threading the Connection
through and having exceptions thrown in my applications.

### Restriction

Lets query a book based on it's title.

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
