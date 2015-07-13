{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8
import Control.Applicative

-- HTTP Request Data Structures
data Method = GET | POST | PUT | DELETE | HEAD | OPTIONS | TRACE | CONNECT deriving (Show)
data ProtocolVersion = HTTP11 | HTTP10 deriving (Show)
data Header = Header { headerName :: String
                     , headerValue :: String }

instance Show Header where
    show (Header name value) = "HEADER [name: " ++ name ++ ", value: " ++ value ++ "]"

data Request = Request { method :: Method
                       , resource :: String
                       , protocolVersion :: ProtocolVersion
                       , headers :: [Header]
                       , body :: String }

instance Show Request where
    show (Request m r p h b) = "REQUEST\n  Method: " ++ show m ++
                               "\n  Resource: " ++ show r ++
                               "\n  ProtocolVersion:" ++ show p ++
                               "\n  Headers: " ++ show h ++
                               "\n  Body: " ++ show (length b) ++ " bytes"

-- simplistic HTTP RequestParser
requestLineParser :: Parser (Method, String, ProtocolVersion)
requestLineParser = do
  method <- methodStringParser
  space
  resource <- many $ notChar ' '
  space
  protocolVersion <- protocolVersionStringParser
  endOfLine
  return $ (method, resource, protocolVersion)

methodStringParser :: Parser Method
methodStringParser =
    ( string "GET" >> return GET )
    <|> (string "POST" >> return POST )
    <|> (string "PUT" >> return PUT )
    <|> (string "DELETE" >> return DELETE )
    <|> (string "HEAD" >> return HEAD )
    <|> (string "OPTIONS" >> return OPTIONS )
    <|> (string "TRACE" >> return TRACE )
    <|> (string "CONNECT" >> return CONNECT )

protocolVersionStringParser :: Parser ProtocolVersion
protocolVersionStringParser =
    ( string "HTTP/1.0" >> return HTTP10 )
    <|> ( string "HTTP/1.1" >> return HTTP11 )

headerLineParser :: Parser Header
headerLineParser = do
  headerName <- many $ notChar ':'
  char ':'
  space
  headerValue <- many $ notChar '\n'
  endOfLine
  return $ Header headerName headerValue

requestParser :: Parser Request
requestParser = do
  (method, resource, protocolVersion) <- requestLineParser
  headers <- many $ headerLineParser
  endOfLine
  body <- many $ notChar '\n'
  return $ Request method resource protocolVersion headers body

main :: IO ()
main = do
  print $ parseOnly requestParser "GET /images/hello.png HTTP/1.1\nfoo: bar\nbaz: bat\n\nfoobar\n\n"
  print $ parseOnly requestParser "GET /images/hello.png HTTP/1.1\nfoo: bar\n\n"
