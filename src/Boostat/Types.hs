{-# LANGUAGE DeriveDataTypeable #-}

module Boostat.Types where

import Data.Time.LocalTime (LocalTime)
import System.Console.CmdArgs (Data, Typeable)

data Args = Args{configFile :: String} deriving (Show, Data, Typeable)

data AppConf = AppConf
  { username :: String
  , password :: String
  , boostId  :: String
  , database :: String
  } deriving (Show)

data Record = Record
  { date        :: LocalTime
  , gesamt      :: Integer
  , offen       :: Integer
  , bestaetigt  :: Integer
  , freigegeben :: Integer
  , ausgezahlt  :: Integer
  } deriving (Show)


