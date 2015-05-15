{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (throwE, ExceptT(..), runExceptT)
import Language.Libconfig.Bindings
import System.Console.CmdArgs
import System.IO

import Boostat.HTTP (getBoost)
import Boostat.SQL (storeData, getData)
import Boostat.Types

main :: IO ()
main = do
  a <- cmdArgs parseArgs
  r <- runExceptT (main' a)
  case r of
    (Right ()) -> return ()
    (Left  e ) -> hPutStrLn stderr $ "error: " ++ e

main' :: Args -> ExceptT String IO ()
main' a = do
  c <- liftIO $ runMaybeT $ readConfig $ configFile a
  conf <- case c of
    Nothing -> throwE "can't read config"
    Just x  -> return x
  currentStat <- liftIO
                 $ getBoost (username conf) (password conf) (boostId conf)
  liftIO $ print currentStat
  storeData (database conf) currentStat
  stats <- getData (database conf)
  liftIO $ print stats

parseArgs :: Args
parseArgs = Args{configFile = "./boostat.conf" &= help "Config file"}
            &= summary "boostat 0.1.0.0"

readConfig :: String -> MaybeT IO AppConf
readConfig f = do
  c <- MaybeT $ configNew f
  u <- MaybeT $ configLookupString c "username"
  p <- MaybeT $ configLookupString c "password"
  b <- MaybeT $ configLookupString c "boostId"
  d <- MaybeT $ configLookupString c "database"
  n <- MaybeT $ configLookupString c "charity"
  return AppConf
    {username = u, password = p, boostId = b
    ,database = d, charity  = n
    }


