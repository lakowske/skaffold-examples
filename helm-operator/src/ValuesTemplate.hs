{-# LANGUAGE OverloadedStrings #-}

module ValuesTemplate where

import Text.Mustache
import qualified Text.Mustache.Types as MT
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text as T
import qualified Data.Aeson as A

writeTemplateWithContext :: T.Text -> FilePath -> (IO T.Text)
writeTemplateWithContext template filePath = do
  let res = compileTemplate "foo"
        "Hi, {{name}}! You have:\n{{#things}}\n  * {{.}}\n{{/things}}\n"
  case res of
    Left bundle -> return $ T.pack (show bundle)
    Right template -> return $ substituteValue template $ MT.mFromJSON (A.object
      [ "name"   A..= ("John" :: T.Text)
      , "things" A..= ["pen" :: T.Text, "candle", "egg"]
      ])

some :: IO T.Text
some = writeTemplateWithContext "blah" "jah" 
-- someFile :: IO T.Text
-- someFile = do
--   content <- B.readFile "./resources/values.yaml"
--   return $ TE.decodeUtf8 content


-- templateToFile :: FilePath -> IO()
-- templateToFile filePath = do
--   content <- someFile  
--   writeTemplate content filePath


-- writeTemplateWithContext :: T.Text -> FilePath -> Context -> IO ()
-- writeTemplateWithContext template filePath tcontext = do
--   S.writeFile filePath (E.encodeUtf8 $ substitute template tcontext)

  
