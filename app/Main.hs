{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Connection (Token)
import Update (InitOpts(..), Stack, init, popError)
import Logic (process)
import Timer (startTimer)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
        stack <- initBot token timeout
        schedule <- process token stack destination
        _ <- startTimer token schedule botName
        errorLoop stack
            where 
                token = ""
                timeout = 10
                destination = ""
                botName = "Хтонь"

initBot :: Token -> Int -> IO Stack
initBot token timeout = Update.init $ InitOpts token timeout

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
