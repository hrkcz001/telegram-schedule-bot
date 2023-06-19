{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Connection (Token)
import Update (InitOpts(..), Stack, init, popError)
import Logic (InitOpts(..), Schedule, process)
import Timer (InitOpts(..), startTimer)
import Control.Concurrent (threadDelay)
import Data.Text (Text)

main :: IO ()
main = do
        stack <- initBot token updateTimeout
        schedule <- initLogic token stack scheduleDefaultInterval destination password admins
        _ <- initTimer token schedule botName delay
        errorLoop stack
            where 
                token = "1023560776:AAE3igMt_MGdw4BYAjAfm2bBesqEBlrR3Hw"
                updateTimeout = 5  -- timeout to get updates in seconds
                scheduleDefaultInterval = 1  -- default interval to send messages in minutes
                delay = 30  -- delay to check next message in schedule in seconds
                destination = "@testhaskell"
                botName = "Хтонь"
                password = "7Qe2bZJ1LG"
                admins = [  "znacit_ja_vcera_u_tebja_gostil"
                         ,  "St_Someone"
                         ]

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
