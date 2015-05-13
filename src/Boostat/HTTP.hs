{-# LANGUAGE OverloadedStrings #-}

module Boostat.HTTP where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (fromMaybe)
import Network (withSocketsDo)
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.PCRE.Light.Char8

getBoost :: String -> String -> String -> IO [Integer]
getBoost u p c = withSocketsDo $ do

  -- get the login form for the authenticity token
  fUrl <- parseUrl "https://www.boost-project.com/de/users/sign_in"
  fRes <- withManager $ httpLbs fUrl
  let jar   = responseCookieJar fRes
  let token = fromMaybe (error "can't find auth token")
              $ pokeToken $ responseBody fRes

  -- Login
  let lPost = loginPostBody u p $ toString token
  let lUrl  = urlEncodedBody lPost $ fUrl {cookieJar = Just jar}
  lRes <- withManager $ httpLbs lUrl

  -- new cookie for logged in session key
  let lJar  = responseCookieJar lRes

  -- fetch and parse project page
  cUrl <- parseUrl $ "https://www.boost-project.com/de/charities/"++c++"/edit"
  cRes <- withManager $ httpLbs $ cUrl {cookieJar = Just lJar}

  -- Logout
  loUrl <- parseUrl "https://www.boost-project.com/de/users/sign_out"
  _ <- withManager $ httpLbs $ loUrl {cookieJar = Just lJar}
  return $ map bsToInt $ pokePrice $ responseBody cRes

loginPostBody :: String -> String -> String
              -> [(BS.ByteString, BS.ByteString)]
loginPostBody u p t =
  [ ("user[email]"       , pack u  )
  , ("user[password]"    , pack p  )
  , ("utf8"              , "&#x2713;")
  , ("authenticity_token", pack t  )
  ]

pokeToken :: BL.ByteString -> Maybe BL.ByteString
pokeToken = pokeToken' . canonicalizeTags . parseTags where
  pokeToken' :: [Tag BL.ByteString] -> Maybe BL.ByteString
  pokeToken' (TagOpen "form" _:TagOpen "input" _:_:TagOpen "input" x:_)
    = lookup "value" x
  pokeToken' (_:ts) = pokeToken' ts
  pokeToken' []     = Just "not found"

pokePrice :: BL.ByteString -> [BL.ByteString]
pokePrice = pokePrice' . canonicalizeTags . parseTags where
  pokePrice' :: [Tag BL.ByteString] -> [BL.ByteString]
  pokePrice' (TagOpen "dt" _:TagText price:ts) = price : pokePrice' ts
  pokePrice' (_:ts)                            = pokePrice' ts
  pokePrice' []                                = []

bsToInt :: BL.ByteString -> Integer
bsToInt b = read $ i++d where
  s = toString b
  r = compile "(\\d+),(\\d\\d)" []
  [_,i,d] = fromMaybe ["0","0","0"] $ match r s []
