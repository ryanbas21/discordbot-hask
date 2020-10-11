-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (fromJSON, parseJSON, withObject, (.:))
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString as B
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text, isPrefixOf, pack, toLower)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Network.Wreq as W
import System.Environment (getEnv)
import UnliftIO

newtype ResponseUrl = ResponseUrl Text

main :: IO ()
main = do
  loadFile defaultConfig
  token <- getEnv "BOT_TOKEN"
  api <- getEnv "GIPHY_API"
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack token,
          discordOnEvent = eventHandler,
          discordOnLog = logger
        }
  TIO.putStrLn userFacingError

logger :: Text -> IO ()
logger txt = do
  print txt
  pure ()

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> handleMessages m
  GuildBanAdd id usr -> pure ()
  _ -> pure ()

createBan :: Either a Guild -> Either a User -> R.CreateGuildBanOpts -> R.GuildRequest ()
createBan (Right (Guild {guildId = guildId})) (Right (User {userId = userId})) = R.CreateGuildBan guildId userId
createBan _ _ = R.CreateGuildBan (Snowflake 0) (Snowflake 0)

getGuild :: Message -> GuildId
getGuild message = case messageGuild message of
  Just id -> id
  Nothing -> fromInteger 0

createGuildBanOpts :: R.CreateGuildBanOpts
createGuildBanOpts = R.CreateGuildBanOpts (Just 0) (Just (pack "Dubbz said so"))

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBan :: Text -> Bool
isBan = ("ban" `isPrefixOf`) . toLower

isMeme :: Text -> Bool
isMeme = ("meme" `isPrefixOf`) . toLower

getMemeApi :: IO String
getMemeApi = getEnv "GIPHY_API"

api :: IO String
api = getEnv "GIPHY_API"

getMeme :: IO (W.Response ResponseUrl)
getMeme = W.asJSON =<< (api >>= (\api -> W.get api))

instance FromJSON ResponseUrl where
  parseJSON =
    withObject "ResponseUrl" $ \v -> do
      theData <- v .: "data"
      url <- theData .: "url"
      pure $ ResponseUrl url

createMessage :: Message -> ResponseUrl -> R.ChannelRequest Message
createMessage msg ((ResponseUrl (url))) = R.CreateMessage (messageId msg) url

handleMessages :: Message -> ReaderT DiscordHandle IO ()
handleMessages msg
  | (not $ fromBot msg && (isMeme . messageText) msg) = do
    giphy <- pure $ getMeme
    url <- pure giphy
    pure $ (\r -> (createMessage msg (r ^. W.responseBody))) <$> url
    pure ()
  | not $ (fromBot msg) && (isBan . messageText) msg = do
    guild <- restCall (R.GetGuild (getGuild msg))
    user <- restCall (R.GetUser (userId $ head $ messageMentions msg)) -- head is not safe.
    pure $ threadDelay (4 * 10 ^ 6)
    _ <- pure $ createBan guild user createGuildBanOpts
    pure ()
  | otherwise = pure ()

