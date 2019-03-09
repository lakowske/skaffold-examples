{-# LANGUAGE OverloadedStrings #-}
module YamlConfigParser where

import Data.Aeson as A
import Data.Aeson.Types
import Data.Yaml.Parser as Y
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Text.Libyaml
import Data.Text
import qualified Data.Map as M

main :: IO ()
main = do
  configmap <- getYamlValue "./src/configmap.yaml"
  putStrLn $ show $ (parse parseData configmap)
  putStrLn $ show configmap

getYamlValue :: FilePath -> IO YamlValue
getYamlValue filePath = readYamlFile filePath

parseData (Mapping vals _) = do
  let configData = lookup "data" vals

  configField <- case configData of
    Just config -> return config
    Nothing -> fail "no field data"
    
  seq <- case configField of
    Sequence s a -> parseSequence (Sequence s a)
    _            -> fail "expected a list"
    
  return seq

parseDeployment (Mapping vals _) = do
  let maybeName = lookup "name" vals

  name <- case maybeName of
    Just (Scalar n _ _ _) -> return n
    Nothing -> fail "deployment missing name"

  let maybeNamespace = lookup "namespace" vals

  namespace <- case maybeNamespace of
    Just (Scalar ns _ _ _) -> return ns
    Nothing -> fail "deployment missing namespace"

  return $ Deployment (C.unpack name) (C.unpack namespace)
    

data Deployment = Deployment {
  name :: String,
  namespace :: String
  } deriving (Show)
  
parseSequence :: YamlValue -> Parser [Deployment]
parseSequence (Sequence vals a) = mapM parseDeployment vals

  
getValMap :: [(Text, YamlValue)] -> M.Map Text YamlValue
getValMap vals = M.fromList vals

          

