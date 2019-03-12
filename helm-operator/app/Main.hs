module Main where

import Cmd
import System.Exit (exitSuccess)
import System.IO
import Control.Concurrent
import ValuesTemplate
import YamlConfigParser
import Data.Aeson.Types

something :: Shell String
something = do
  getResource "seth" >>= toNamespace "joe"
  
monitorChart :: IO ()
monitorChart = do
  let releaseName = "micro1"
  let valuesFile = "/opt/server/resources/deployedValues.yaml"
  let releaseList = "/tmp/helmreleaselist.yaml"
  
  readShell $ createNamespace "seth"
  helmReleases <- readShell $ getNamespacedResourceYaml "helmreleases" "default"
  writeFile "/tmp/helmreleaselist.yaml" helmReleases
  list <- getYamlValue releaseList
  putStrLn (show list)

  templateToFile valuesFile

  template <- someFile
  -- do
  --   (Success deployments) <- return (parse (parseResource parseDeploymentAsMap) list)
  --   sequence $ (map (\context -> (writeTemplateWithContext template valuesFile context)) deployments)

  -- putStrLn $ show $ deployments
  
  
  exists <- helmContains releaseName
  failed <- helmFailed releaseName


  result <- case failed of
    True  -> readShell $ helmDelete releaseName
    False -> return "Not failed"

  putStrLn result
  
  result <- case exists of
    True -> return "Installed"
    False -> do
      putStrLn "installing..."

      readShell $ helmInstall "lakowske/micro-registry" ("--name " ++ releaseName ++ " --values " ++ valuesFile)
      
  putStrLn result
  
  hFlush stdout      
  threadDelay 2000000
  monitorChart
  
main :: IO ()
main = do
  putStrLn "Starting seth ops :)"
  hFlush stdout
  monitorChart
  

--  (readShell $ deleteConfigMap "animal")
--  (readShell $ createConfigMap "animal" [("name", "jody")])
--  putStrLn "Hello, Kubernetes!"

