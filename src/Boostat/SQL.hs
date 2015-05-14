module Boostat.SQL where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)

storeData :: String -> [Integer] -> ExceptT String IO ()
storeData db [a,b,c,d,e] = do
  c <- openDb db
  return ()
storeData _  _           = throwE "can't read boost stats"

getData :: String -> ExceptT String IO [[Integer]]
getData db = throwE "db read not implemented yet"

openDb :: String -> ExceptT String IO Connection
openDb db = hdbcToExcept ("can't connect to " ++ db) $ connectSqlite3 db

hdbcToExcept :: String -> IO a -> ExceptT String IO a
hdbcToExcept s e = ExceptT
  $ handleSql (const $ return $ Left s) $ liftM Right e
