{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Gemini.Server
import Network.Gemini.Router
import Language.Gemini

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Control.Concurrent (forkIO, threadDelay)

import Network.URI (parseRelativeReference)

import Data.Functor (($>))
import Data.Foldable (asum)
import Data.Traversable (for)
import Data.Maybe (fromJust)
import Data.List (intercalate)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Database.SQLite.Simple as SQL
import qualified Data.Cache as Cache
import System.Clock (TimeSpec(TimeSpec))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale, rfc822DateFormat)
import qualified Crypto.Nonce as Nonce
import Data.ByteString.Base64.URL (encodeBase64Unpadded)
import OpenSSL.X509 (writeDerX509)
import Crypto.Hash.SHA256 (hashlazy)


data Context = Context
  { db :: SQL.Connection
  , gen :: Nonce.Generator
  , cache :: Cache.Cache Text PostType }

type PostId = Int

data PostType = New | Reply PostId

data Post = Post
  { postId :: PostId
  , postContent :: Text
  , postAuthor :: Maybe Text
  , postTime :: UTCTime }

instance SQL.FromRow Post where
  fromRow = Post <$> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field

type App = RouteT (ReaderT Context IO)

main :: IO ()
main = do
  conn <- SQL.open "gemini-textboard.db"
  createTables conn
  nonceGen <- Nonce.new
  nonceCache <- Cache.newCache $ Just $ TimeSpec 300 0
  let cleanNonceCache = do
        threadDelay 300000000
        Cache.purgeExpired nonceCache
        cleanNonceCache
  _ <- forkIO cleanNonceCache
  let ctx = Context conn nonceGen nonceCache
  runServer Nothing "1965" "cert.pem" $ runRouteT' (`runReaderT` ctx) app

-- DB stuff
-----------

createTables :: SQL.Connection -> IO ()
createTables conn = do
  SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS posts " <>
    "(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, " <>
    "parent INTEGER, content TEXT NOT NULL, author TEXT, time TEXT NOT NULL, " <>
    "FOREIGN KEY(parent) REFERENCES posts(id))"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS post_id_index ON posts (id)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS post_parent_index ON posts (parent)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS post_time_index ON posts (time)"

getAllThreads :: App [(Post, Maybe Post)]
getAllThreads = do
  conn <- db <$> lift ask
  results <- liftIO $ SQL.query_ conn
    "SELECT p.id, p.content, p.author, p.time, r.id, r.content, r.author, r.time \
    \  FROM\
    \    posts AS p\
    \    OUTER LEFT JOIN (SELECT *, max(time) FROM posts GROUP BY parent) AS r\
    \  ON p.id = r.parent\
    \  WHERE p.parent IS NULL\
    \  ORDER BY IFNULL(r.time, p.time) DESC"
  pure $ fmap mkPosts results
  where mkPosts (opId, opTxt, opAuthor, opTime, lastId, lastTxt, lastAuthor, lastTime) =
          (Post opId opTxt opAuthor opTime, Post <$> lastId <*> lastTxt <*> Just lastAuthor <*> lastTime)


getThread :: PostId -> App (Maybe Post)
getThread threadId = do
  conn <- db <$> lift ask
  liftIO $ headMaybe <$> SQL.query conn
    "SELECT id, content, author, time FROM posts WHERE id = ? AND parent IS NULL" (SQL.Only threadId)

getReplies :: PostId -> App [Post]
getReplies threadId = do
  conn <- db <$> lift ask
  liftIO $ SQL.query conn
    "SELECT id, content, author, time FROM posts WHERE parent = ?" (SQL.Only threadId)

insertThread :: String -> App PostId
insertThread content = do
  conn <- db <$> lift ask
  now <- liftIO getCurrentTime
  author <- getAuthor
  liftIO $ SQL.execute conn
    "INSERT INTO posts (parent, content, author, time) VALUES (NULL,?,?,?)"
    (content, author, now)
  fmap fromIntegral $ liftIO $ SQL.lastInsertRowId conn --TODO non thread safe?

insertReply :: PostId -> String -> App ()
insertReply threadId content = do
  conn <- db <$> lift ask
  now <- liftIO getCurrentTime
  author <- getAuthor
  liftIO $ SQL.execute conn
    "INSERT INTO posts (parent, content, author, time) VALUES (?,?,?,?)"
    (threadId, content, author, now)

-- Nonce stuff
--------------

newNonce :: PostType -> App Text
newNonce postType = do
  ctx <- lift ask
  nonce <- Nonce.nonce128urlT $ gen ctx
  liftIO $ Cache.insert (cache ctx) nonce postType
  pure nonce

validateNonce :: Text -> App (Maybe PostType)
validateNonce nonce = do
  ctx <- lift ask
  liftIO $ Cache.lookup (cache ctx) nonce

-- Obtaining an author id
-------------------------

getAuthor :: App (Maybe Text)
getAuthor = do
  maybeCert <- requestCert <$> getRequest
  for maybeCert $ \c -> do
    -- Huge hack, but it works reasonably well.
    -- Ideally one should only hash the pk parameters.
    der <- liftIO $ writeDerX509 c
    pure $ encodeBase64Unpadded $ hashlazy der

-- Handlers
-----------

app :: App Response
app = asum
  [ end homepageHandler
  , dir "post" $ end newThreadRedirectHandler
  , dir "thread" $ capture $ \threadId -> end $ threadHandler threadId
  , dir "thread" $ capture $ \threadId -> dir "post" $ end $ threadPostRedirectHandler threadId
  , dir "p" $ capture $ \nonce -> input "Write your post" $ \post -> end $ postHandler nonce post
  , dir "robots.txt" $ end robotsTxt
  ]

homepageHandler :: App Response
homepageHandler = do
  threads <- getAllThreads
  pure $ okGemini $ encodeUtf8 $ encodeGemini $
    [ LH1 "Gemini Textboard"
    , LLink "/post" $ Just "New thread"
    , LText ""
    ] <> intercalate [LText ""] (renderThread <$> threads)

renderThread :: (Post, Maybe Post) -> GeminiDocument
renderThread (op, lastReply) =
  renderOp op <>
  [LText "⋮"] <>
  maybe [LText "No replies yet"] renderReply lastReply <>
  [LLink (LT.pack $ "/thread/" <> show (postId op)) $ Just "Go to thread"]

threadHandler :: String -> App Response
threadHandler threadId' = do
  let threadId = read threadId' :: Int --TODO better parsing --TODO check existence
  Just op <- getThread threadId
  replies <- getReplies threadId
  pure $ okGemini $ encodeUtf8 $ encodeGemini $
    [ LH1 "Gemini Textboard"
    , LLink "/" $ Just "Back to thread list"
    , LText ""
    ] <> renderOp op <> [LText ""] <>
    intercalate [LText ""] (renderReply <$> replies) <>
    [LText "", LLink ("/thread/" <> LT.pack (show threadId) <> "/post") $ Just "New reply"]

renderOp :: Post -> GeminiDocument
renderOp (Post i txt author t) =
  [ LH2 $ "#" <> LT.pack (show i) <>
          " - " <> renderAuthor author <> " - " <>
          LT.pack (formatTime defaultTimeLocale rfc822DateFormat t)
  , LText $ LT.fromStrict txt
  ]

renderReply :: Post -> GeminiDocument
renderReply (Post i txt author t) =
  [ LH3 $ "#" <> LT.pack (show i) <>
          " - " <> renderAuthor author <> " - " <>
          LT.pack (formatTime defaultTimeLocale rfc822DateFormat t)
  , LText $ LT.fromStrict txt
  ]

renderAuthor :: Maybe Text -> LT.Text
renderAuthor = maybe "Anonymous" LT.fromStrict

threadPostRedirectHandler :: String -> App Response
threadPostRedirectHandler threadId' = do
  let threadId = read threadId' --TODO better parsing --TODO check existence
  nonce <- newNonce $ Reply threadId
  pure $ redirect $ fromJust $ parseRelativeReference $ "/p/" <> T.unpack nonce

newThreadRedirectHandler :: App Response
newThreadRedirectHandler = do
  nonce <- newNonce New
  pure $ redirect $ fromJust $ parseRelativeReference $ "/p/" <> T.unpack nonce

postHandler :: String -> String -> App Response
postHandler nonce content = do
  Just postType <- validateNonce (T.pack nonce) --TODO decent error handling (or is MonadFail ok?)
  threadId <- case postType of
    New -> insertThread content
    Reply threadId -> insertReply threadId content $> threadId
  pure $ redirect $ fromJust $ parseRelativeReference $ "/thread/" <> show threadId

robotsTxt :: App Response
robotsTxt = pure $ okGemini $ encodeUtf8
  "User-agent: *\n\
  \# Do not crawl submission pages\n\
  \Disallow: /post/\n\
  \Disallow: /p/"

-- Utils
--------

headMaybe :: [a] -> Maybe a
headMaybe (a:_) = Just a
headMaybe []    = Nothing
