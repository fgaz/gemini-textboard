{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Gemini.Server
import Network.Gemini.Router
import Language.Gemini

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Network.URI (parseRelativeReference)

import Data.Functor (($>))
import Data.Foldable (asum)
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
import qualified Crypto.Nonce as Nonce


data Context = Context
  { db :: SQL.Connection
  , gen :: Nonce.Generator
  , cache :: Cache.Cache Text PostType }

type ThreadId = Int

data PostType = New | Reply ThreadId

type App = RouteT (ReaderT Context IO)

main :: IO ()
main = do
  conn <- SQL.open "gemini-textboard.db"
  createTables conn
  nonceGen <- Nonce.new
  nonceCache <- Cache.newCache $ Just $ TimeSpec 300 0 -- TODO clean periodically
  let ctx = Context conn nonceGen nonceCache
  runServer Nothing "1964" $ runRouteT' (`runReaderT` ctx) app

-- DB stuff
-----------

createTables :: SQL.Connection -> IO ()
createTables conn = do
  SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS posts " <>
    "(id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, " <>
    "parent INTEGER, content TEXT NOT NULL, time TEXT NOT NULL, " <>
    "FOREIGN KEY(parent) REFERENCES posts(id))"
  SQL.execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS post_id_index ON posts (id)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS post_parent_index ON posts (parent)"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS post_time_index ON posts (time)"

getAllThreads :: App [(Int, Text, UTCTime, Maybe (Int, Text, UTCTime))]
getAllThreads = do
  conn <- db <$> lift ask
  results <- liftIO $ SQL.query_ conn
    "SELECT p.id, p.content, p.time, r.id, r.content, r.time \
    \  FROM\
    \    posts AS p\
    \    OUTER LEFT JOIN (SELECT *, max(time) FROM posts GROUP BY parent) AS r\
    \  ON p.id = r.parent\
    \  WHERE p.parent IS NULL\
    \  ORDER BY IFNULL(r.time, p.time) DESC"
  pure $ fmap groupLast results
  where groupLast (opId, opTxt, opTime, lastId, lastTxt, lastTime) =
          (opId, opTxt, opTime, (,,) <$> lastId <*> lastTxt <*> lastTime)


getThread :: ThreadId -> App (Maybe (Text, UTCTime))
getThread threadId = do
  conn <- db <$> lift ask
  liftIO $ headMaybe <$> SQL.query conn
    "SELECT content, time FROM posts WHERE id = ? AND parent IS NULL" (SQL.Only threadId)

getReplies :: ThreadId -> App [(Int, Text, UTCTime)]
getReplies threadId = do
  conn <- db <$> lift ask
  liftIO $ SQL.query conn
    "SELECT id, content, time FROM posts WHERE parent = ?" (SQL.Only threadId)

insertThread :: String -> App ThreadId
insertThread content = do
  conn <- db <$> lift ask
  now <- liftIO getCurrentTime
  liftIO $ SQL.execute conn
    "INSERT INTO posts (parent, content, time) VALUES (NULL,?,?)" (content, now)
  fmap fromIntegral $ liftIO $ SQL.lastInsertRowId conn --TODO non thread safe?

insertReply :: ThreadId -> String -> App ()
insertReply threadId content = do
  conn <- db <$> lift ask
  now <- liftIO getCurrentTime
  liftIO $ SQL.execute conn
    "INSERT INTO posts (parent, content, time) VALUES (?,?,?)"
    (threadId, content, now)

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

-- Handlers
-----------

app :: App Response
app = asum
  [ end homepageHandler
  , dir "post" $ end newThreadRedirectHandler
  , dir "thread" $ capture $ \threadId -> end $ threadHandler threadId
  , dir "thread" $ capture $ \threadId -> dir "post" $ end $ threadPostRedirectHandler threadId
  , dir "p" $ capture $ \nonce -> input "Write your post" $ \post -> end $ postHandler nonce post
  ]

homepageHandler :: App Response
homepageHandler = do
  threads <- getAllThreads
  pure $ okGemini $ encodeUtf8 $ encodeGemini $
    [ LH1 "Gemini Textboard"
    , LLink "/post" $ Just "New thread"
    , LText ""
    ] <> intercalate [LText ""] (renderThread <$> threads)

renderThread :: ( ThreadId, T.Text, UTCTime
                , Maybe (Int, T.Text, UTCTime) ) -> GeminiDocument
renderThread (i, txt, t, lastReply) =
  renderOp (i, txt, t) <>
  [LText "⋮"] <>
  maybe [LText "No replies yet"] renderReply lastReply <>
  [LLink (LT.pack $ "/thread/" <> show i) $ Just "Go to thread"]

threadHandler :: String -> App Response
threadHandler threadId' = do
  let threadId = read threadId' :: Int --TODO better parsing --TODO check existence
  Just (op, opTime) <- getThread threadId
  replies <- getReplies threadId
  pure $ okGemini $ encodeUtf8 $ encodeGemini $
    [ LH1 "Gemini Textboard"
    , LLink "/" $ Just "Back to thread list"
    , LText ""
    ] <> renderOp (threadId, op, opTime) <> [LText ""] <>
    intercalate [LText ""] (renderReply <$> replies) <>
    [LText "", LLink ("/thread/" <> LT.pack (show threadId) <> "/post") $ Just "New reply"]

renderOp :: (ThreadId, T.Text, UTCTime) -> GeminiDocument
renderOp (i, txt, t) =
  [ LH2 $ "#" <> LT.pack (show i) <>
          " - Anonymous - " <> --TODO use client cert for "tripcode"
          LT.pack (show t)
  , LText $ LT.fromStrict txt
  ]

renderReply :: (Int, T.Text, UTCTime) -> GeminiDocument
renderReply (i, txt, t) =
  [ LH3 $ "#" <> LT.pack (show i) <>
          " - Anonymous - " <> --TODO use client cert for "tripcode"
          LT.pack (show t)
  , LText $ LT.fromStrict txt
  ]

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

-- Utils
--------

headMaybe :: [a] -> Maybe a
headMaybe (a:_) = Just a
headMaybe []    = Nothing

