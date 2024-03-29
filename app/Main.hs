{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main (main) where

import Connection (Token)
import Update (InitOpts(..), Stack, init, popError)
import Logic (InitOpts(..), Schedule, process)
import Timer (InitOpts(..), startTimer)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson (FromJSON(..), withObject, (.:))

data Options = Options Token Int Int Int Text Text Text [Text]

instance FromJSON Options where
    parseJSON = withObject "Options" $ \v -> Options
        <$>
        v .: "token" <*>
        v .: "updateTimeout" <*>
        v .: "scheduleDefaultInterval" <*>
        v .: "delay" <*>
        v .: "destination" <*>
        v .: "botName" <*>
        v .: "password" <*>
        v .: "admins"

main :: IO ()
main = do
        options <- decodeFileEither "config.yaml" :: IO (Either ParseException Options)
        case options of
            Left err -> print err
            Right (Options token updateTimeout scheduleDefaultInterval delay destination botName password admins) -> do
                stack <- initBot token updateTimeout
                schedule <- initLogic token stack scheduleDefaultInterval destination password admins
                _ <- initTimer token schedule botName delay
                errorLoop stack

initBot :: Token -> Int -> IO Stack
initBot token timeout = Update.init $ Update.InitOpts token timeout

initLogic :: Token -> Stack -> Int -> Text -> Text -> [Text] -> IO Schedule
initLogic token stack interval destination password admins = 
    Logic.process $ Logic.InitOpts stack token interval destination password admins

initTimer :: Token -> Schedule -> Text -> Int -> IO ()
initTimer token schedule defaultName delay = Timer.startTimer $ Timer.InitOpts token schedule defaultName delay

errorLoop :: Stack -> IO ()
errorLoop stack = do
                    err <- popError stack
                    case err of
                        Nothing  -> do
                                    threadDelay 1000000
                                    errorLoop stack
                        Just val -> do
                                    print val
                                    errorLoop stack
