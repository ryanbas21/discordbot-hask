-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Applicative (liftA2)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Lens
import Control.Monad (forever, liftM, void, when)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (fromJSON, parseJSON, withObject, (.:))
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text, isPrefixOf, isSuffixOf, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Debug.Trace
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Network.Wreq as W
import System.Environment (getEnv)
import UnliftIO

newtype ResponseUrl = ResponseUrl Text deriving (Show)

instance FromJSON ResponseUrl where
  parseJSON =
    withObject "ResponseUrl" $ \v -> do
      theData <- v .: "data"
      url <- theData .: "url"
      pure $ ResponseUrl url

main :: IO ()
main = do
  loadFile defaultConfig
  token <- getEnv "BOT_TOKEN"
  outChan <- newChan :: IO (Chan String)

  -- Events are processed in new threads, but stdout isn't
  -- synchronized. We get ugly output when multiple threads
  -- write to stdout at the same time
  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn

  userFacingError <-
    runDiscord $
      def
        { discordToken = pack token,
          discordOnEvent = eventHandler outChan,
          discordOnLog = logger
        }
  TIO.putStrLn userFacingError

logger :: Text -> IO ()
logger txt = do
  pure $ trace "tracing::: " $ unpack txt
  pure ()

eventHandler :: Chan String -> Event -> DiscordHandler ()
eventHandler out event = do
  liftIO $ writeChan out (show event <> "\n")
  case event of
    MessageCreate m -> handleMessages m
    GuildBanAdd id usr -> pure ()
    _ -> pure ()

-- utils
fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBan :: Text -> Bool
isBan = ("ban" `isSuffixOf`) . toLower

isMeme :: Text -> Bool
isMeme = ("meme" `isSuffixOf`) . toLower

isPrefix :: Message -> Bool
isPrefix = ("!" `isPrefixOf`) . messageText

getMemeApi :: IO String
getMemeApi = getEnv "GIPHY_API"

api :: IO String
api = getEnv "GIPHY_API"

getUserIdFromMentions :: Message -> UserId
getUserIdFromMentions = (userId . head . messageMentions)

getMeme :: IO (W.Response ResponseUrl)
getMeme = api >>= W.get >>= W.asJSON

--
createBan :: Either a Guild -> Either a User -> R.CreateGuildBanOpts -> R.GuildRequest ()
createBan (Right (Guild {guildId = guildId})) (Right (User {userId = userId})) = R.CreateGuildBan guildId userId
createBan _ _ = R.CreateGuildBan (Snowflake 0) (Snowflake 0)

getGuild :: Message -> GuildId
getGuild message = case messageGuild message of
  Just id -> id
  Nothing -> fromInteger 0

createGuildBanOpts :: R.CreateGuildBanOpts
createGuildBanOpts = R.CreateGuildBanOpts (Just 0) (Just (pack "Dubbz said so"))

createMessage :: Message -> ResponseUrl -> DiscordHandler (Either RestCallErrorCode Message)
createMessage msg (ResponseUrl (url)) = restCall $ R.CreateMessage (messageChannel msg) url

handleIOMeme :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleIOMeme msg = lift getMeme >>= (\a -> createMessage msg (a ^. W.responseBody))

callPong :: Message -> DiscordHandler (Either RestCallErrorCode Message)
callPong msg = restCall (R.CreateMessage (messageChannel msg) "Pong!")

handleMessages :: Message -> DiscordHandler ()
handleMessages msg
  | isPrefix msg && (not . fromBot) msg && (isMeme . messageText) msg = do
    handleIOMeme msg
    pure ()
  | isPrefix msg && (not . fromBot) msg && (isBan . messageText) msg = do
    guild <- restCall $ R.GetGuild $ getGuild msg
    user <- restCall $ R.GetUser $ getUserIdFromMentions msg -- head is not safe.
    restCall $ createBan guild user createGuildBanOpts
    pure ()
  | otherwise = do
    pure $ putStrLn "here "
    pure ()
