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
import Data.Text (Text, append, isPrefixOf, isSuffixOf, pack, tail, takeWhile, takeWhileEnd, toLower, unpack)
import qualified Data.Text.IO as TIO
import Debug.Trace
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Network.Wreq as W
import System.Environment (getEnv)
import Twitter
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
  token <- getEnv "LEAGUE_TOKEN"
  outChan <- newChan :: IO (Chan String)

  -- Events are processed in new threads, but stdout isn't
  -- synchronized. We get ugly output when multiple threads
  -- write to stdout at the same time
  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn
  twitter
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
fromBot = (userIsBot . messageAuthor)

isVanity :: Text -> Bool
isVanity = isSuffixOf ("vanity")

isUnban :: Text -> Bool
isUnban str = Data.Text.takeWhile ((/=) ' ') (Data.Text.tail str) == pack "unban"

isBan :: Text -> Bool
isBan str = Data.Text.takeWhile ((/=) ' ') (Data.Text.tail str) == pack "ban"

isKick :: Text -> Bool
isKick str = Data.Text.takeWhile ((/=) ' ') (Data.Text.tail str) == pack "kick"

getTextAfterCommand :: Text -> Text
getTextAfterCommand = Data.Text.takeWhileEnd ((/=) ' ')

isMeme :: Text -> Bool
isMeme = ("meme" `isSuffixOf`) . toLower

rokers :: Message -> Bool
rokers = (("rokers" `isSuffixOf`) . toLower . messageText)

isPrefix :: Message -> Bool
isPrefix = ("!" `isPrefixOf`) . messageText

minion :: Message -> Bool
minion = (("minion" `isSuffixOf`) . toLower . messageText)

getMemeApi :: IO String
getMemeApi = getEnv "GIPHY_API"

api :: IO String
api = getEnv "GIPHY_API"

getUserIdFromMentions :: Message -> UserId
getUserIdFromMentions msg = case (length $ messageMentions msg) > 0 of
  True -> (userId . head . messageMentions) msg
  False -> Snowflake 0

getMeme :: IO (W.Response ResponseUrl)
getMeme = api >>= W.get >>= W.asJSON

--
createBan :: Either a Guild -> Either a User -> R.CreateGuildBanOpts -> R.GuildRequest ()
createBan (Right (Guild {guildId = guildId})) (Right (User {userId = userId})) = R.CreateGuildBan guildId userId
createBan _ _ = R.CreateGuildBan (Snowflake 0) (Snowflake 0)

getGuildId :: Message -> GuildId
getGuildId message = case messageGuild message of
  Just id -> id
  Nothing -> fromInteger 0

createGuildBanOpts :: Either RestCallErrorCode User -> R.CreateGuildBanOpts
createGuildBanOpts (Right user) = R.CreateGuildBanOpts Nothing (Just (append (pack "Banned ") (userName user)))
createGuildBanOpts (Left err) = R.CreateGuildBanOpts Nothing (Just (append (pack "Error ") ("No username")))

createMessage :: Message -> ResponseUrl -> DiscordHandler (Either RestCallErrorCode Message)
createMessage msg (ResponseUrl (url)) = restCall $ R.CreateMessage (messageChannel msg) url

handleIOMeme :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleIOMeme msg = lift getMeme >>= (\a -> createMessage msg (a ^. W.responseBody))

callPong :: Message -> DiscordHandler (Either RestCallErrorCode Message)
callPong msg = restCall (R.CreateMessage (messageChannel msg) "Pong!")

handleMessages :: Message -> DiscordHandler ()
handleMessages msg
  | (not $ fromBot msg) && (isPrefix msg) && minion msg = do
    restCall $ R.CreateMessage (messageChannel msg) "https://imgur.com/j0He7b6"
    pure ()
  | (not $ fromBot msg) && (isPrefix msg) && (rokers msg) = do
    restCall $ R.CreateMessage (messageChannel msg) "https://imgur.com/JiQMASG"
    pure ()
  | ((messageText msg) == (pack "pong")) = do
    restCall $ R.CreateMessage (messageChannel msg) "Ping"
    pure ()
  | isPrefix msg && (not . fromBot) msg && (isMeme . messageText) msg = do
    handleIOMeme msg
    pure ()
  | isPrefix msg && (not . fromBot) msg && (isBan . messageText) msg = do
    guild <- restCall $ R.GetGuild $ getGuildId msg
    user <- restCall $ R.GetUser $ getUserIdFromMentions msg -- head is not safe.
    restCall $ R.CreateMessage (messageChannel msg) "here"
    restCall $ createBan guild user (createGuildBanOpts user)
    pure ()
  | isPrefix msg && (not . fromBot) msg && (isVanity . messageText) msg = do
    restCall $ R.GetGuildVanityURL (getGuildId msg)
    pure ()
  | isPrefix msg && (not . fromBot) msg && (isKick . messageText) msg = do
    restCall $ R.RemoveGuildMember (getGuildId msg) (getUserIdFromMentions msg)
    pure ()
  | isPrefix msg && (not . fromBot) msg && (isUnban . messageText) msg = do
    restCall $ R.RemoveGuildBan (getGuildId msg) (getUserIdFromMentions msg)
    pure ()
  | otherwise = do
    pure $ putStrLn "here "
    pure ()
