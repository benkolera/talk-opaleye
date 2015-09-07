# OpalLib - Demo Opaleye Code & Tutorial

## Installation & Running Instructions

You don't need to run the code to follow along, but if you want to see things
work / inspect types / tinker then you may want to set things up.

```
psql postgres -c "CREATE DATABASE opallib" && psql opallib -f setup.sql

cabal sandbox init
cabal install --only-dependencies
```

You'll need to edit the connectInfo hardcoded in

[OpalLib.Util](OpalLib/Util.hs)

And then you'll be able to run / play in the repl

```
# Will print out the queries and results from Main.hs
cabal run 

# Will get a repl that you can play with things, inspect types
cabal repl
```

## Why Opaleye

## Table Definition

## Querying

## Inserts / Updates

## Aggregation

## Extending Opaleye

## Acknowledgements & Further Resources

Thanks to [https://github.com/tomjaguarpaw/](Tom Ellis) for the lovely library
and all the hard work that others have done on HaskellDB (a precursor to
Opaleye).

Hackage: http://hackage.haskell.org/package/opaleye

Github:  https://github.com/tomjaguarpaw/haskell-opaleye
