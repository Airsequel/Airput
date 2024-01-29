module FileUploader (uploadFiles)
where

import Protolude (
  Either (Left, Right),
  IO,
  Maybe (Just),
  Show,
  filterM,
  flip,
  pure,
  putErrLn,
  putStrLn,
  ($),
  (&),
  (.),
  (/=),
  (<&>),
  (<>),
  (||),
 )

import Control.Arrow ((>>>))
import Control.Monad ((=<<))
import Data.Aeson (encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (lookup)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector (toList)
import GHC.Generics (Generic)
import Network.HTTP.Client (
  Request (method, requestBody, requestHeaders),
  RequestBody (RequestBodyLBS),
  Response (responseBody, responseStatus),
  httpLbs,
  newManager,
  parseRequest,
 )
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (statusMessage), methodPut)
import Protolude qualified as P
import System.Directory (doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (FilePath, takeBaseName, takeExtension, (</>))


-- | Escape double quotes in SQL strings
escDoubleQuotes :: Text -> Text
escDoubleQuotes =
  T.replace "\"" "\"\""


-- | Quote a keyword in an SQL query
quoteKeyword :: Text -> Text
quoteKeyword keyword =
  keyword
    & escDoubleQuotes
    & (\word -> "\"" <> word <> "\"")


-- | Escape single quotes in SQL strings
escSingleQuotes :: Text -> Text
escSingleQuotes =
  T.replace "'" "''"


-- | Quote literal text in an SQL query
quoteText :: Text -> Text
quoteText keyword =
  keyword
    & escSingleQuotes
    & (\word -> "'" <> word <> "'")


data FileData = FileData
  { name :: FilePath
  , song_id :: Int
  , filetype :: Text
  }
  deriving (Generic, Show)


getFilesSorted :: FilePath -> IO [FilePath]
getFilesSorted path = do
  filePaths <- listDirectory path
  filePaths
    & filterM (doesFileExist . (path </>))
    <&> ( P.filter (/= ".DS_Store")
            >>> P.sort
        )


createSQLQuery :: Text -> FileData -> Text
createSQLQuery tableName fileData =
  ("INSERT INTO " <> quoteKeyword tableName <> " (name, filetype) ")
    <> ( "VALUES ("
          <> quoteText (T.pack $ takeBaseName fileData.name)
          <> ", "
          <> quoteText fileData.filetype
          <> ") "
       )
    <> "RETURNING rowid"


uploadFiles :: Text -> Text -> Text -> [FilePath] -> IO ()
uploadFiles domain dbId tableName paths = do
  manager <- newManager tlsManagerSettings

  fileLists <- P.forM paths $ \filePath -> do
    isFile <- doesFileExist filePath

    if isFile
      then pure [filePath]
      else getFilesSorted filePath

  P.forM_ (P.concat fileLists) $ \fileName -> do
    let
      fileData =
        FileData
          { name = fileName
          , song_id = 0
          , filetype = T.drop 1 $ T.pack $ takeExtension fileName
          }
      url :: Text = domain <> "/dbs/" <> dbId <> "/sql"

    initialRequest <- parseRequest $ T.unpack url

    let
      query = createSQLQuery tableName fileData
      sqlRequest =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode $ object ["query" .= query]
          , requestHeaders = [("Content-Type", "application/json")]
          }

    sqlResponse <- httpLbs sqlRequest manager

    let resBody = sqlResponse.responseBody & BSL.toStrict

    if (sqlResponse.responseStatus.statusMessage /= "OK")
      || ( ("error" `BS.isInfixOf` resBody)
            P.&& P.not ("errors\":[]" `BS.isInfixOf` resBody)
         )
      then
        putErrLn $
          "ERROR:\n"
            <> ("File entry \"" <> fileName <> "\" could not be inserted.\n")
            <> P.show sqlResponse
      else
        putStrLn $
          "Inserted file \"" <> fileName <> "\" into SQLite database."

    let
      body = sqlResponse.responseBody & BSL.toStrict

      -- Extract rowid from json response with aeson
      rowidResult :: Either Text BS.ByteString
      rowidResult =
        case Aeson.decode $ BSL.fromStrict body of
          Just (Aeson.Object obj) -> case obj & lookup "rows" of
            Just (Aeson.Array x) -> case Vector.toList x of
              (Aeson.Object y) : _ -> case y & lookup "rowid" of
                Just (Aeson.Number z) -> Right $ BSL.toStrict $ encode z
                _ -> Left "Could not find rowid in row"
              _ -> Left "Rows is not an array"
            _ -> Left "Could not find rows in response"
          _ -> Left "Could not decode response"

    case rowidResult of
      Left err -> do
        putErrLn err
        pure ()
      Right rowid -> do
        -- Upload file
        initialFileRequest <-
          parseRequest $
            T.unpack $
              domain
                <> "/api/dbs/"
                <> dbId
                <> "/tables/"
                <> tableName
                <> "/columns/content/files/rowid/"
                <> (rowid & unpack & T.pack)

        filePathAbs <- makeAbsolute fileName

        let
          fileRequest =
            formDataBody
              [partFileSource "file" filePathAbs]
              initialFileRequest

        fileRes <-
          flip httpLbs manager
            =<< (fileRequest <&> (\req -> req{method = methodPut}))

        if (fileRes.responseStatus.statusMessage /= "OK")
          || ("error" `BS.isInfixOf` (fileRes.responseBody & BSL.toStrict))
          then
            putErrLn $
              "ERROR:\n"
                <> ("File \"" <> fileName <> "\" could not be uploaded.\n")
                <> P.show fileRes
          else
            putStrLn $
              "Uploaded file \"" <> fileName <> "\""
