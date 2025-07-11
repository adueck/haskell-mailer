module FileHelpers
  ( getFileFromForm,
  )
where

import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as B
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent, fileName)
import Web.Scotty

getFileFromForm :: ActionM (Maybe (String, B.ByteString))
getFileFromForm = do
  filesOpts defaultParseRequestBodyOptions $ \_ fs -> do
    let fs' = [(fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName, fi) <- fs]
    if null fs'
      then return Nothing
      else do
        let (_, fnam, fpath) = head fs'
        -- TODO Error handle this
        fc <- do liftIO (try (B.readFile fpath) :: IO (Either SomeException B.ByteString))
        case fc of
          Left _ -> return Nothing
          Right fContent -> return $ Just (fnam, fContent)
