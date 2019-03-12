{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module YamlConfigParser where

import Data.Aeson as A
import Data.Aeson.Types
import Data.Yaml as YM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text
import qualified Data.Map as M
import Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import ValuesTemplate

parseTest2 :: IO ()
parseTest2 = do
  list <- B.readFile "./resources/helmreleaselist.yaml"
  print list
  case (decodeEither' list) of
    Left (problem::ParseException) -> print problem
    Right (val :: Value) -> print (parse (parseList (parseResource parseDeployment)) val)

-- parseTest :: IO ()
-- parseTest = do
--   list <- readYamlFile "./resources/helmreleaselist.yaml"
--   print list
--   print (parse (parseList (parseResource parseDeployment)) list)

parseSequence :: Value -> (Value -> Parser a) -> Parser [a]
parseSequence (Array vals) f = mapM f (V.toList vals)
parseSequence _ _ = fail "not a sequence"

parseList :: (Value -> Parser t) -> Value -> Parser [t]
parseList f (Object vals) = do
  let itemsData = HM.lookup "items" vals

  case itemsData of
    Just s -> parseSequence s f
    Nothing -> fail "no items"
parseList _ _ = fail "not an object"

parseResource :: (Value -> Parser a) -> Value -> Parser a
parseResource f (Object vals) = do
  let configData = HM.lookup "spec" vals

  case configData of
    Just config -> f config
    Nothing -> fail "no spec data"
parseResource _ _ = fail "not an object"

parseDeployment :: Value -> Parser Deployment
parseDeployment = parseJSON


data Deployment = Deployment {
  name :: String,
  namespace :: String,
  host :: String
  } deriving (Show)

instance ToJSON Deployment where
  toJSON (Deployment name namespace host)   =
    object ["name" .= name, "namespace" .= namespace, "host" .= host]

instance FromJSON Deployment where
  parseJSON = withObject "Deployment" $ \v -> Deployment
    <$> v YM..: "name"
    <*> v YM..: "namespace"
    <*> v YM..: "host"





