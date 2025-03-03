module SimpleTemplate
  ( renderTemplate,
  )
where

import Data.Text (Text, pack, replace)

-- | takes a path to a text file, and a list of (key,value) pairs
-- for replacing replaces each {{key}} in the file with it's value
renderTemplate :: FilePath -> [(String, String)] -> IO Text
renderTemplate path replacements = do
  htmlTemplate <- readFile path
  return $
    foldl
      (\temp (key, val) -> replace (pack ("{{" ++ key ++ "}}")) (pack val) temp)
      (pack htmlTemplate)
      replacements
