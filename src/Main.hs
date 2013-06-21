import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)
import           Data.Conduit
import qualified Data.Conduit.List          as CL
import qualified Data.Conduit.Text          as CT
import           Data.Function              (on)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (sortBy)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.IO          as TIO
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Word                  (Word)
import           Network.HTTP.Conduit
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.HashMap.Strict as HashMap

-- | The main entry point.
main :: IO ()
main = forever $ do
    putStrLn "Starting parse"
    parseLog
    putStrLn "Sleeping"
    threadDelay $ 1000 * 1000 * 60 * 30 -- 30 minutes

data UploadStats = UploadStats
    { usPackages :: !(HashMap PackageName UploadCount)
    , usUsers    :: !(HashMap UserName UploadCount)
    }
    deriving Show

type PackageName = Text
type UserName = Text
type UploadCount = Word

instance Monoid UploadStats where
    mempty = UploadStats mempty mempty
    mappend (UploadStats a x) (UploadStats b y) = UploadStats
        (HashMap.unionWith (+) a b)
        (HashMap.unionWith (+) x y)

parseLog :: IO ()
parseLog = do
    now <- getCurrentTime
    req <- parseUrl "http://hackage.haskell.org/packages/archive/log"
    withManager $ \man -> do
        res <- http req man
        us <- responseBody res
            $$+- CT.decode CT.utf8
             =$ CT.lines
             =$ CL.fold addUpload mempty
        liftIO $ putStrLn "Parse succeeded, uploading"
        let upload = def
                { method = "POST"
                , secure = True
                , host = "api.github.com"
                , port = 443
                , path = "/gists"
                , requestBody = RequestBodyLBS $ encode $ object
                    [ "description" .= ("Hackage upload log @ " ++ show now)
                    , "public" .= True
                    , "files" .= object
                        [ "upload-log.txt" .= object
                            [ "content" .= toLazyText (prettyUploadStats now us)
                            ]
                        ]
                    ]
                , requestHeaders =
                    [ ("User-Agent", "http-conduit")
                    ]
                }
        res2 <- httpLbs upload man
        liftIO $ TIO.putStrLn $ case decode $ responseBody res2 of
            Just (Object o)
              | Just (String htmlUrl) <- HashMap.lookup "html_url" o ->
                "Uploaded to: " <> htmlUrl
              | otherwise -> "Couldn't find html_url: " <> T.pack (show o)
            Nothing -> "Invalid JSON response: " <> TL.toStrict (decodeUtf8With lenientDecode $ responseBody res2)
                
  where
    addUpload us = mappend us . parseUpload
    parseUpload t =
        case reverse $ T.words t of
            _version:package:user:_rest -> UploadStats
                (HashMap.singleton package 1)
                (HashMap.singleton user 1)
            _ -> mempty

prettyUploadStats :: UTCTime -> UploadStats -> Builder
prettyUploadStats now (UploadStats packages users) =
    fromString (show now) <> go "\n\nPackages\n" packages <> go "\nUsers\n" users
  where
    go title m = mconcat
               $ title
               : map oneStat (sortBy (flip compare `on` snd) $ HashMap.toList m)
    oneStat (name, count) = fromText name <> ": " <> decimal count <> "\n"