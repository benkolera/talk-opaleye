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
bookTable = Table "book" . pBook $ Book
  { _bookIsbn  = pIsbn . Isbn $ required "isbn"
  , _bookTitle = required "title"
  }
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
type PersonIdColumnMaybe = PersonId' (Maybe (Column PGInt4))

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
type IsbnColumn  = Isbn' (Column PGInt8)
type BookColumns = Book' IsbnColumn (Column PGText)

type Isbn = Isbn' Int64
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

Lets query a book based on its ISBN.

```haskell
findBookByIsbnQ :: IsbnColumn -> Query BookColumns
findBookByIsbnQ isbn = proc () -> do
   b <- bookQuery -< ()
   restrict -< unIsbn (b^.bookIsbn) .== unIsbn isbn
   returnA -< b
```

Lets ignore this funky syntax for now and focus on what's happening:

- We start with all books
- Then restrict them to only the ones which have an Isbn that matches our input.
- We return the books that we found.

Generated SQL:

```sql
SELECT "isbn0_1" as "result1_2",
       "title1_1" as "result2_2"
FROM (SELECT *
      FROM (SELECT "isbn" as "isbn0_1",
                   "title" as "title1_1"
            FROM "book" as "T1") as "T1"
      WHERE (("isbn0_1") = 9781593272838)) as "T1"
```

Then to run the query and return a book if there is one:

```haskell
findBookByIsbn :: CanOpaleye c e m => Isbn -> m (Maybe Book)
findBookByIsbn = liftQueryFirst . findBookByIsbnQ . constant
```

Constant is the opposite of QueryRunnerDefault. It goes from a haskell value to
a literal column(s). Here we are turning the Isbn with a haskell Int64 inside it
to a Isbn' (Column PGInt8).

More info in [OpalLib.Book](OpalLib/Book.hs).

### Arrows

We don't need to get into the nitty gritty details of arrows but it is
worthwhile noting some points.

The query generation is purposefully not a monad and (at least my intuition) is
that the arbitrary nesting of monadic computation gets you into trouble by
allowing you to refer to things that aren't actually valid any more (because
they are in a subquery and weren't projected/aggregated out of the query, etc).

The main thing to note about arrows is that they are much more like Applicative
in that the computation branches out but doesn't nest (no join), so things only
flow from one context to the other.

In the arrow syntax, the arrow has Input, does some work with that input (or
variable in scope out of the arrow comprehension entirely) and produces an
output. That means that each arrow has complete control over what columns
further queries can see and use later on down the track, which gives us a lot of
our safety!

Side note, Query is just an alias for a QueryArr with no input:
type Query a = QueryArr () a

### Projection

Remember that the only real restriction we have on the output of our query is
that it is a product profunctor, so you're not limited to always returning your
table object. You're free to return tuples or just a single column.

```haskell
bookTitlesQuery :: Query (Column PGText)
bookTitlesQuery = proc () -> do
  b <- bookQuery -< ()
  returnA -< b^.bookTitle

bookTitles :: CanOpaleye c e m => m [Text]
bookTitles = liftQuery bookTitlesQuery
```

```sql
SELECT "title1_1" as "result1_2"
FROM (SELECT *
      FROM (SELECT "isbn" as "isbn0_1",
                   "title" as "title1_1"
            FROM "book" as "T1") as "T1") as "T1"
```

### Join

Joins are as simple as putting two tables in your arrow comprehension and
restricting them as appropriate.

```haskell
booksWithKeywordQuery :: Column PGText -> Query BookColumns
booksWithKeywordQuery kw = proc () -> do
  b <- bookQuery        -< ()
  k <- bookKeywordQuery -< ()
  restrict -< b^.bookIsbn.to unIsbn .== k^.bookKeywordBookIsbn.to unIsbn
  restrict -< k^.bookKeywordKeyword .== kw
  returnA -< b
```

```sql
SELECT "isbn0_1" as "result1_3",
       "title1_1" as "result2_3"
FROM (SELECT *
      FROM (SELECT "isbn" as "isbn0_1",
                   "title" as "title1_1"
            FROM "book" as "T1") as "T1",
           (SELECT "book_isbn" as "book_isbn0_2",
                   "keyword" as "keyword1_2"
            FROM "book_keyword" as "T1") as "T2"
      WHERE (("keyword1_2") = E'Programming') AND (("isbn0_1") = ("book_isbn0_2"))) as "T1"
```

### Reusing Joins & Restrictions

But the above example would get clunky if you had to join to keyword lots. Lets
refactor it out so that we can reuse it elsewhere.

```haskell
booksWithKeywordQuery :: Column PGText -> Query BookColumns
booksWithKeywordQuery kw = proc () -> do
  b  <- bookQuery       -< ()
  k  <- bookKeywordJoin -< (b)
  restrict -< k .== kw
  returnA -< b
  
bookKeywordJoin :: QueryArr BookColumns (Column PGText)
bookKeywordJoin = proc (b) -> do
  k <- bookKeywordQuery -< ()
  restrict -< b^.bookIsbn.to unIsbn .== k^.bookKeywordBookIsbn.to unIsbn
  returnA -< k^.bookKeywordKeyword
```

And if we are restricting lots based on keywords we can even make that part
reusable:

```haskell
bookRestrictedByKeyword
  :: Column PGText
  -> QueryArr BookColumns (Column PGText)
bookRestrictedByKeyword kw = proc (b) -> do
  k <- bookKeywordJoin -< b
  restrict -< k .== kw
  returnA -< k

booksWithKeywordQuery :: Column PGText -> Query BookColumns
booksWithKeywordQuery kw = proc () -> do
  b <- bookQuery -< ()
  bookRestrictedByKeyword kw -< b
  returnA -< b
```

All of these generate exactly the same SQL; which is awesome! 

More info in [OpalLib.Book](OpalLib/Book.hs).

## Inserts / Updates

Generating queries is great, but we are going to need to insert and change data
at some point! We'll experiment with this by writing a function to loan
accessions (an copy of a book) out.

See [OpalLib.Loan](OpalLib/Loan.hs) for the full code.

### insert

```haskell
borrow
  :: CanOpaleye c e m
  => AccessionId 
  -> PersonId
  -> UTCTime
  -> UTCTime
  -> m LoanId
borrow aId pId b d =
  fmap head $ liftInsertReturning loanTable (^.loanId) $ Loan
    { _loanId          = LoanId Nothing
    , _loanPersonId    = constant pId
    , _loanAccessionId = constant aId
    , _loanBorrowed    = constant b
    , _loanDue         = constant d
    , _loanReturned    = null
    }
```

Lets break this down from right to left:

- We create a Loan to insert with a default id and a null returned date
- We insert that into the loan table returning the new id
- We are only every gonna get one and only one of them but liftInsertReturning
  returns a list; so fix that. :wink:

Generates the SQL that you'd expect:

```sql
INSERT INTO loan (id,person_id,accession_id,borrowed,due,returned)
VALUES (null,1,1,'2015-08-01','2015-09-01',null)
RETURNING id
```

### update

```haskell
loanReturn :: CanOpaleye c e m => LoanId -> UTCTime -> m ()
loanReturn lId r = void $ liftUpdate loanTable
  (  (loanId %~ pLoanId (LoanId Just))
   . (loanReturned .~ toNullable (constant r))
  )
  (\ l -> l^.loanId.to unLoanId .== unLoanId (constant lId))
```

From right to left:

- Updating the loan table where the id = the input loan
- The 2nd argument to liftUpdate is a function from LoanColumns ->
  LoanInsertColumns so we have to wrap the current loan id in Just as well as
  set the returned date that we wanted to do.

```(%~)``` and ```(.~)``` are lens functions that modify the current value with
a function and set the current value to a constant (respectively). To learn
more, delve into the wonderful world of [lens](http://hackage.haskell.org/package/lens)

## Aggregation

Aggregation is one of the things that make SQL an awesome reporting / data
processing language. And the fact that opaleye manages to offer all the
aggregate functions without sacrificing any type safety is really the cream on
the cake of this awesome DSL. :smile:

### count

Lets start off with a simple count of accessions.

```haskell
accessionsForBookQuery :: IsbnColumn -> Query (AccessionIdColumn,BookColumns)
accessionsForBookQuery isbn = proc () -> do
  t <- accessionsWithBookQuery -< ()
  restrict -< t^._2.bookIsbn.to unIsbn .== unIsbn isbn
  returnA -< t

accessionCountForBookQuery :: IsbnColumn -> Query (Column PGInt8)
accessionCountForBookQuery =
  aggregate count . fmap (^._1.to unAccessionId) . accessionsForBookQuery
```

aggregate takes an Aggregate a and a Query a, and is a product profunctor
so if there are more than one columns you can supply and aggregate function for
each hole with an appropriate pAccession or pN call.

Also don't forget that Querys are Functors so we can just fmap the result rather
than breaking out into arrow syntax. They are also Profunctors,Applicatives and
even Categories (you can compose them e.g. ```QueryArr a b -> QueryArr b c ->
QueryArr a c). Haskell is awesome! 

Generated SQL:

```sql
SELECT "result0_3" as "result1_4"
FROM (SELECT *
      FROM (SELECT COUNT("id0_1") as "result0_3"
            FROM (SELECT *
                  FROM (SELECT "id" as "id0_1",
                               "book_isbn" as "book_isbn1_1"
                        FROM "accession" as "T1") as "T1",
                       (SELECT "isbn" as "isbn0_2",
                               "title" as "title1_2"
                        FROM "book" as "T1") as "T2"
                  WHERE (("isbn0_2") = 9781593272838) AND (("book_isbn1_1") = ("isbn0_2"))) as "T1"
            GROUP BY COALESCE(0)) as "T1") as "T1"
```

### groupBy

Now to group by is as simple as returning multiple columns and grouping by the
ones that you want.

```haskell
accessionCountsForKeywordQuery :: Query (Column PGText,Column PGInt8)
accessionCountsForKeywordQuery =
  orderBy (desc (^._2)) . aggregate (p2 (groupBy,count)) $ proc () -> do
    (aId,b) <- accessionsWithBookQuery -< ()
    k       <- bookKeywordJoin -< b
    returnA -< (k,aId^.to unAccessionId)
```

```sql
SELECT "result0_4" as "result1_5",
       "result1_4" as "result2_5"
FROM (SELECT *
      FROM (SELECT *
            FROM (SELECT *
                  FROM (SELECT "keyword1_3" as "result0_4",
                               COUNT("id0_1") as "result1_4"
                        FROM (SELECT *
                              FROM (SELECT "id" as "id0_1",
                                           "book_isbn" as "book_isbn1_1"
                                    FROM "accession" as "T1") as "T1",
                                   (SELECT "isbn" as "isbn0_2",
                                           "title" as "title1_2"
                                    FROM "book" as "T1") as "T2",
                                   (SELECT "book_isbn" as "book_isbn0_3",
                                           "keyword" as "keyword1_3"
                                    FROM "book_keyword" as "T1") as "T3"
                              WHERE (("isbn0_2") = ("book_isbn0_3")) AND (("book_isbn1_1") = ("isbn0_2"))) as "T1"
                        GROUP BY "keyword1_3") as "T1") as "T1"
            ORDER BY "result1_4" DESC NULLS FIRST) as "T1") as "T1"
```

### Other Aggregate Functions

There are lots of other aggregate functions defined in
[Opaleye.Aggregate](http://hackage.haskell.org/package/opaleye-0.4.1.0/docs/Opaleye-Aggregate.html):

- sum
- avg
- max
- min
- boolOr    (any)
- boolAnd   (all)
- arrayAgg  (Column a -> Column (PGArray a))
- stringAgg (array_to_string)

### pagination

Pagination is a very common thing that otherwise needs lots of repetition or
ugly hacks. Wouldn't it be great if you you write it once for any query and
repeat easily compose it into any query?

Lets define our pagination input and results.

```haskell
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
```

We can simply limit and offset any Query a with functions from opaleye:

```haskell
paginateQuery :: Pagination -> Query a -> Query a
paginateQuery (Pagination p w) = limit w . offset ((p-1) * w)
```

And we know how to count a column, so we can count any query if we have a way to
get a countable column from it.

```haskell
countQuery :: (a -> Column i) -> Query a -> Query (Column PGInt8)
countQuery getId = aggregate count . fmap getId
```

And then gluing these together into grab the current page and the full total is
a piece of cake.

```haskell
paginate
  :: (CanOpaleye c e m, Default QueryRunner a b)
  => Pagination
  -> (a -> Column i)
  -> Query a
  -> m (PaginationResults b)
paginate p getId q = paginationResults p
  <$> liftQueryFirst (countQuery getId q)
  <*> liftQuery      (paginateQuery p q)
```

Mixing this into a query is easy.

```haskell
booksWithKeywordPaginated
  :: CanOpaleye c e m
  => Pagination
  -> Text
  -> m (PaginationResults Book)
booksWithKeywordPaginated p
  = paginate p (^.bookIsbn.to unIsbn) . booksWithKeywordQuery . constant

-- The old version was : booksWithKeywordQuery . constant
```

See [OpalLib.Pagination](OpalLib/Pagination.hs) for the full story and
[OpalLib.Book](OpalLib/Book.hs) for the paginated book call.

## The Big Finale

The most hideous part of many a webapps is usually some search or report that
has to configurably search on different fields. Lets give this a whirl in OpalLib.

Our search terms:

```haskell
data Search = Search
  { _searchTitle         :: Maybe Text
  , _searchKeywords      :: [Text]
  , _searchAvailableOnly :: Bool
  }
makeLenses ''Search
```

First we are going to need a gnarly left join to the Loan table to get a
nullable due date (if there is a current loan against the accession).

```haskell
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
```

Then do query full of all of the accession ids and array of keywords:

```haskell
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
```

(Okay, so granted that's poorly factored and we start to see Arrows get in the way of
our usual toolbox [namely not being able to use Data.Foldable.traverse_], but it
is still getting some reuse out of what we've written and could be refactored to
be prettier)

Then we can easily get back the accessionId,book,due date and keywords
reusing a lot of stuff.

```haskell
searchQuery :: Search -> Query SearchResultColumns
searchQuery s = proc () -> do
  (a,b,l)   <- accessionWithDueDateQuery -< ()
  (aId,kws) <- searchAccessionIdQuery s -< ()
  restrict  -< unAccessionId aId .== unAccessionId a
  returnA   -< SearchResult a b kws l
```

And hey, we can even paginate it without a hassle:

```haskell
searchPaginated
  :: CanOpaleye c e m
  => Pagination
  -> Search
  -> m (PaginationResults SearchResult)
searchPaginated p s = paginate p
  (^.searchResultAccessionId.to unAccessionId)
  (searchQuery s)
```

Boom! :wink:

Details in [OpalLib.Search](OpalLib/Search.hs)

## Room For Improvement

I've talked about the good parts, but there are a number of bits that have room
for improvement in Opaleye.

These could be my inexperience or they could be just be because someone hasn't
needed them yet. Given that opaleye is still pretty young I'm sure some of these
have some clever fixes.

### The schema on the DB could be different

Nothing out of the box checks the schema on boot of your programing / in test cases,
so if your tables aren't there or you messed up the table/column names your stuff will
break at runtime.

A really cool thing would be something that you could give a list of tables to and it'd
go and check that the tables actually line up. Something that you could do when your app
started up so it could squawk if the DB wasn't using the right schema before it goes live.

### Dates

There is no function equivalent to NOW(). It's easy to write though:

```haskell
now :: Column PGTimestamptz
now = C.Column (HPQ.FunExpr "now" [])
```

Nor is there any support for infinite timestamps (Unbounded in pg-simple) or intervals.

There is also no nice way to compare dates and timestamps without ugly coercion.
It needs some love like what already exists for PGOrd.

### Cardinality

It's sometimes easy to introduce joins to many relationships that blow up the
number of results that you're expecting back.

It would kinda be nice to have relationships modelable so that you could get an
idea of the cardinality in the types, but I think that would break the
simplicity of things.

### Newtypes

Wrapping things in newtypes introduces some extra ceremony with the product
profunctor parts where we don't necessarily need the extra hassle. It's minor
compared to the gains though.

Also, the template haskell for makeAdaptorAndInstance won't accept a newtype, so
you have to pay the runtime price of a full blown datatype with a single
constructor. 

### Varchar Lengths

It's really easy to bust up an insert / update if the Text exceeds the VARCHAR
length. It'd be nice if there was a way to make this more explicit in the types.

### Like Escaping

With the like operator, it's a bit weird that it takes a raw PGText. It means
you'd have to remember to escape % signs in user input yourself, which seems
like it could be better.

### Using a DB that isn't PostgreSQL

But why would you do that? :wink:

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
