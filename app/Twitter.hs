-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Twitter (twitter) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as S8
import Data.Maybe
import System.Environment
import System.IO (hFlush, stdout)
import UnliftIO
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit hiding (lookup)
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Types (User)

getTokens :: IO OAuth
getTokens = do
  consumerKey <- getEnv "OAUTH_CONSUMER_KEY"
  consumerSecret <- getEnv "OAUTH_CONSUMER_SECRET"
  accessToken <- getEnv "TWITTER_BEARER_TOKEN"
  return $
    twitterOAuth
      { oauthConsumerKey = S8.pack consumerKey,
        oauthConsumerSecret = S8.pack consumerSecret,
        oauthCallback = Just "oob"
      }

authorize ::
  -- | OAuth Consumer key and secret
  OAuth ->
  Manager ->
  IO OAuth
authorize oauth mgr = do
  cred <- OA.getTemporaryCredential oauth mgr
  tok <- pure $ defaultAccessTokenRequest oauth cred mgr
  pure $ accessTokenOAuth tok

twInfo :: Credential -> IO TWInfo
twInfo cred =
  getTokens
    >>= (\a -> pure $ setCredential a cred (def {twProxy = Nothing}))

manager :: IO Manager
manager = newManager tlsManagerSettings

getScreenName :: IO TWInfo -> IO User
getScreenName info = join $ call <$> info <*> manager <*> (pure $ usersShow $ ScreenNameParam "thimura")

twitter :: IO ()
twitter = do
  oauth <- authorize <$> getTokens <*> manager
  cred <- OA.getTemporaryCredential <$> oauth <*> manager
  info <- twInfo <$> cred
  getScreenName info
  pure ()

