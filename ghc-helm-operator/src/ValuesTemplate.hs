{-# LANGUAGE OverloadedStrings #-}

module ValuesTemplate where

import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as E

import Data.Text.Template

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

someFile :: IO T.Text
someFile = do
  content <- B.readFile "./resources/values.yaml"
  return $ TE.decodeUtf8 content


templateToFile :: FilePath -> IO()
templateToFile filePath = do
  content <- someFile  
  writeTemplate content filePath

writeTemplate :: T.Text -> FilePath -> IO ()
writeTemplate template filePath = do
  S.writeFile filePath (E.encodeUtf8 $ substitute template helloContext)
  where
    helloContext  = context [("host", "minikube.st81ess.com")]

  
printTemplate :: T.Text -> IO ()
printTemplate template = S.putStr $ E.encodeUtf8 $ substitute template helloContext
  where
    helloContext  = context [("host", "minikube.st81ess.com")]
