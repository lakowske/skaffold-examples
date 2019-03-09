module Main where

import Cmd
import System.Exit (exitSuccess)
import System.IO
import Control.Concurrent
import ValuesTemplate

something :: Shell String
something = do
  getResource "seth" >>= toNamespace "joe"
  
monitorChart :: IO ()
monitorChart = do
  let releaseName = "micro1"
  let valuesFile = "/opt/server/resources/deployedValues.yaml"
  readShell $ createNamespace "seth"
  helmReleases <- readShell $ getNamespacedResourceYaml "helmreleases" "default"
  putStrLn helmReleases
  
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
      templateToFile valuesFile
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

