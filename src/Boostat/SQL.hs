module Boostat.SQL where

import Control.Monad.Trans.Except (ExceptT(..), throwE)

store :: String -> [Integer] -> ExceptT String IO ()
store db [a,b,c,d,e] = throwE "db store not implemented yet"
store _  _           = throwE "can't read boost stats"

read :: String -> ExceptT String IO [[Integer]]
read db = throwE "db read not implemented yet"
