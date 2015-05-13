{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (throwE, ExceptT(..), runExceptT)
import Language.Libconfig.Bindings
import System.Console.CmdArgs

import Boostat.HTTP (getBoost)

data Args = Args{configFile :: String} deriving (Show, Data, Typeable)

data AppConf = AppConf
  { username :: String
  , password :: String
  , boostId  :: String
  , database :: String
  } deriving (Show)

main :: IO ()
main = do
  a <- cmdArgs parseArgs
  r <- runExceptT (main' a)
  case r of
    (Right ()) -> return ()
    (Left  e ) -> print e

main' :: Args -> ExceptT String IO ()
main' a = do
  c <- liftIO $ runMaybeT $ readConfig $ configFile a
  conf <- case c of
    Nothing -> throwE "can't read config"
    Just x  -> return x
  liftIO $ print conf
  return ()
  liftIO $ print =<< getBoost (username conf) (password conf) (boostId conf)




parseArgs :: Args
parseArgs = Args{configFile = "./boostat.conf" &= help "Config file"}
            &= summary "boostat 0.0.1"

readConfig :: String -> MaybeT IO AppConf
readConfig f = do
  c <- MaybeT $ configNew f
  u <- MaybeT $ configLookupString c "username"
  p <- MaybeT $ configLookupString c "password"
  b <- MaybeT $ configLookupString c "boostId"
  d <- MaybeT $ configLookupString c "database"
  return AppConf{username = u, password = p, boostId = b, database = d}


