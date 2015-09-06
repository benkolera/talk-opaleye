{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module OpalLib.Util where

import Control.Applicative             ((<$>))
import Control.Monad.Reader            (ReaderT,runReaderT)
import Control.Monad.Except            (ExceptT,runExceptT)
import Data.Monoid                     ((<>))
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (ConnectInfo(..),connect,defaultConnectInfo)
import Opaleye                         (Unpackspec,Query,showSqlForPostgres)
import Opaleye.Classy                  (OpaleyeEnv(..),OpaleyeError)
import Text.Groom

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

ourConnectInfo :: ConnectInfo
ourConnectInfo = defaultConnectInfo 
  { connectDatabase = "opallib" 
  , connectUser     = "ben" 
  }

type Opaleye = ReaderT OpaleyeEnv (ExceptT OpaleyeError IO)

printOpaleye :: Show a => Opaleye a -> IO ()
printOpaleye o= do
  res <- runOpaleye o
  putStrLn $ case res of
    Left err -> "ERROR: " <> show err
    Right a  -> groom a

runOpaleye :: Opaleye a -> IO (Either OpaleyeError a)
runOpaleye o = OpaleyeEnv <$> connect ourConnectInfo >>= runExceptT . runReaderT o

opaleyeExample
  :: (Default Unpackspec a a,Show b)
  => String
  -> Query a
  -> Opaleye b
  -> IO ()
opaleyeExample title q o = do
  printTitle title
  printSql q
  printThinLine
  printOpaleye o
  printEnding

printEnding :: IO ()
printEnding = do
  printThinLine
  putStrLn ""

printTitle :: String -> IO ()
printTitle title = printThinLine >> putStrLn title >> printThinLine

printThinLine :: IO ()
printThinLine = putStrLn $ line '-'

line :: Char -> String
line = take 80 . repeat
