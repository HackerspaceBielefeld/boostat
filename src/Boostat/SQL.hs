module Boostat.SQL where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)

import Boostat.Types (Record)

insQuery :: String
insQuery = "INSERT INTO boostat "
        ++ "(date, gesamt, offen, bestaetigt, freigegeben, ausgezahlt) "
        ++ "VALUES (DATETIME(\"NOW\"),?,?,?,?,?)"

selQuery :: String
selQuery = "SELECT (date, gesamt, offen, bestaetigt, freigegeben, ausgezahlt) "
        ++ "FROM boostat "
        ++ "WHERE date >= DATETIME(\"NOW\" \"-30 days\")"

storeData :: String -> [Integer] -> ExceptT String IO ()
storeData db r@[_,_,_,_,_] = do
  c <- openDb db
  hdbcToExcept "can't write to DB: " $ do
    run c insQuery (map toSql r)
    commit c
storeData _  _           = throwE "can't read boost stats"

getData :: String -> ExceptT String IO [[Integer]]
getData db = do
  c <- openDb db
  hdbcToExcept "can't read from DB: " $ do

openDb :: String -> ExceptT String IO Connection
openDb db = hdbcToExcept ("can't connect to " ++ db ++ ": ")
  $ connectSqlite3 db

hdbcToExcept :: String -> IO a -> ExceptT String IO a
hdbcToExcept s a = ExceptT
  $ handleSql (\e -> return $ Left $ s ++ seErrorMsg e) $ liftM Right a
