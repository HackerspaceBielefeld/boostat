module Boostat.SQL where

import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)

storeData :: String -> [Integer] -> ExceptT String IO ()
storeData db [a,b,c,d,e] = throwE "db store not implemented yet"
storeData _  _           = throwE "can't read boost stats"

getData :: String -> ExceptT String IO [[Integer]]
getData db = throwE "db read not implemented yet"


openDb :: String -> ExceptT String IO ()
openDb db = return ()

