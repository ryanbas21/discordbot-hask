-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Twitter (twitter) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as S8
import System.Environment
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit
    ( Manager,
      newManager,
      tlsManagerSettings,
      usersShow,
      call,
      setCredential,
      twitterOAuth,
      UserParam(ScreenNameParam),
      TWInfo(twProxy) )
import Web.Twitter.Types (User)

getTokens :: IO OAuth
getTokens = do
  consumerKey <- getEnv "OAUTH_CONSUMER_KEY"
  consumerSecret <- getEnv "OAUTH_CONSUMER_SECRET"
  -- accessToken <- getEnv "TWITTER_BEARER_TOKEN"
  return $
    twitterOAuth
      { oauthConsumerKey = S8.pack consumerKey,
        oauthConsumerSecret = S8.pack consumerSecret,
        oauthCallback = Just "oob"
      }

twInfo :: Credential -> IO TWInfo
twInfo cred =
  getTokens
    >>= (\a -> pure $ setCredential a cred (def {twProxy = Nothing}))

manager :: IO Manager
manager = newManager tlsManagerSettings

getScreenName :: IO TWInfo -> IO User
getScreenName info = join $ call <$> info <*> manager <*> (pure $ usersShow $ ScreenNameParam "ryanbas21")

credential :: IO Credential
credential = do 
  oauth_token <- S8.pack <$> getEnv "TWITTER_ACCESS_TOKEN"
  oauth_secret <- S8.pack <$> getEnv "TWITTER_ACCESS_SECRET"
  pure $ Credential
    [ ("oauth_token", oauth_token)
    , ("oauth_token_secret", oauth_secret)
    ]

twitter :: IO ()
twitter = do
  loadFile defaultConfig
  info <- twInfo <$> credential 
  req <- getScreenName info
  print req
  pure ()

