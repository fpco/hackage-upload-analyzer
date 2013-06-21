{-# LANGUAGE OverloadedStrings #-}
module Main where

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
--import qualified Data.Text.ICU              as ICU
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Lazy.IO          as TLIO
import           Data.Time                  (UTCTime, getCurrentTime)
import           Data.Word                  (Word)
import           Network.HTTP.Conduit

-- | The main entry point.
main :: IO ()
main = forever $ do
    parseLog
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
    us <- withManager $ \man -> do
        res <- http req man
        responseBody res
            $$+- CT.decode CT.utf8
             =$ CT.lines
             =$ CL.fold addUpload mempty
    TLIO.putStr $ toLazyText $ prettyUploadStats now us
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
--               : map oneStat (sortBy (smartCompare `on` fst) $ HashMap.toList m)
               : map oneStat (sortBy (flip compare `on` snd) $ HashMap.toList m)
    oneStat (name, count) = fromText name <> ": " <> decimal count <> "\n"
    --smartCompare = ICU.compare [ICU.CompareIgnoreCase]
    --smartCompare = compare