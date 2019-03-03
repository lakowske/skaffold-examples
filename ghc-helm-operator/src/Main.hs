module Main where

import Cmd

main :: IO ()
main = do
  installed <- helmContains "blog1"
  case installed of
    True -> do
      readShell $ helmInstall "lakowske/blog" "--name blog1"
      putStrLn "installing..."
    False -> do
      result <- (readShell $ helmList)
      putStrLn result
--  (readShell $ deleteConfigMap "animal")
--  (readShell $ createConfigMap "animal" [("name", "jody")])
--  putStrLn "Hello, Kubernetes!"

