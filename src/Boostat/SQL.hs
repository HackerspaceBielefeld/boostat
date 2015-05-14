module Boostat.SQL where

import Control.Applicative ((<$>))
import Control.Monad (liftM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import System.Directory (doesFileExist)

import Boostat.Types (Record(..))

insQuery :: String
insQuery = "INSERT INTO boostat "
        ++ "(date, gesamt, offen, bestaetigt, freigegeben, ausgezahlt) "
        ++ "VALUES (DATETIME(\"NOW\"),?,?,?,?,?)"

selQuery :: String
selQuery = "SELECT date, gesamt, offen, bestaetigt, freigegeben, ausgezahlt "
        ++ "FROM boostat "
        ++ "WHERE date >= DATETIME(\"NOW\", \"-30 days\")"

storeData :: String -> [Integer] -> ExceptT String IO ()
storeData db r@[_,_,_,_,_] = do
  c <- openDb db
  hdbcToExcept "can't write to DB: " $ do
    run c insQuery (map toSql r)
    commit c
storeData _  _           = throwE "can't read boost stats"

getData :: String -> ExceptT String IO [Record]
getData db = do
  c <- openDb db
  hdbcToExcept "can't read from DB: "
    $ map convertRecord <$> quickQuery c selQuery []

openDb :: String -> ExceptT String IO Connection
openDb db = do
  fTest <- liftIO $ doesFileExist db
  unless fTest $ throwE $ "DB file " ++ db ++ " does not exist"
  hdbcToExcept ("can't connect to " ++ db ++ ": ") $ connectSqlite3 db

convertRecord :: [SqlValue] -> Record
convertRecord [a,b,c,d,e,f] = Record
  { date       = fromSql a, gesamt      = fromSql b, offen      = fromSql c
  , bestaetigt = fromSql d, freigegeben = fromSql e, ausgezahlt = fromSql f
  }

hdbcToExcept :: String -> IO a -> ExceptT String IO a
hdbcToExcept s a = ExceptT
  $ handleSql (\e -> return $ Left $ s ++ seErrorMsg e) $ liftM Right a
