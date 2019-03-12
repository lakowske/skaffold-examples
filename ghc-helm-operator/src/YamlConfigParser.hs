{-# LANGUAGE OverloadedStrings #-}
module YamlConfigParser where

import Data.Aeson as A
import Data.Aeson.Types
import Data.Yaml.Parser as Y
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text
import qualified Data.Map as M
import Data.Text.Template
import qualified Data.Text as T

import ValuesTemplate

parseTest :: IO ()
parseTest = do
  list <- getYamlValue "./resources/helmreleaselist.yaml"
  putStrLn $ show list  
  putStrLn $ show $ (parse (parseList (parseResource parseDeployment)) list)


getYamlValue :: FilePath -> IO YamlValue
getYamlValue filePath = readYamlFile filePath

parseSequence :: YamlValue -> (YamlValue -> Parser a) -> Parser [a]
parseSequence (Sequence vals anchor) f = mapM f vals
parseSequence _ _ = fail "not a sequence"

parseList :: (YamlValue -> Parser t) -> YamlValue -> Parser [t]
parseList f (Mapping vals _) = do
  let itemsData = lookup "items" vals
  
  case itemsData of
    Just s -> parseSequence s f
    Nothing -> fail "no items"


parseResource :: (YamlValue -> Parser a) -> YamlValue -> Parser a
parseResource f (Mapping vals _) = do
  let configData = lookup "spec" vals

  case configData of
    Just config -> f config
    Nothing -> fail "no spec data"
    

parseDeployment :: YamlValue -> Parser Deployment
parseDeployment (Mapping vals _) = do
  let maybeName = lookup "name" vals

  name <- case maybeName of
    Just (Scalar n _ _ _) -> return n
    Nothing -> fail "deployment missing name"

  let maybeNamespace = lookup "namespace" vals

  namespace <- case maybeNamespace of
    Just (Scalar ns _ _ _) -> return ns
    Nothing -> fail "deployment missing namespace"

  let maybeHost = lookup "host" vals
  host <- case maybeHost of
    Just (Scalar h _ _ _) -> return h
    Nothing -> fail "missing host"
    
  return $ Deployment (C.unpack name) (C.unpack namespace) (C.unpack host)
parseDeployment x = error (" not a mapping" ++ (show x))

parseDeploymentAsMap :: YamlValue -> Parser Context
parseDeploymentAsMap val = do
  dep <- parseDeployment val
  return $ context [("name", T.pack $ name dep), ("namespace", T.pack $ namespace dep), ("host", T.pack $ host dep)]
    
    

data Deployment = Deployment {
  name :: String,
  namespace :: String,
  host :: String
  } deriving (Show)
  

  
getValMap :: [(Text, YamlValue)] -> M.Map Text YamlValue
getValMap vals = M.fromList vals

          

