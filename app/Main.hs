{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Connection (Token, getUpdate, copyMessage, changeName)
import Update (Error(..), Schedule, ScheduledMessage(..), State(..), initialState, handleResponse, Msg2Copy(..))
import Logic (proccessMessage)
import Control.Concurrent
import Control.Lens
import Network.Wreq
import Data.Time.Clock.POSIX (getPOSIXTime)

updateLoop :: Token -> State -> IO ()
updateLoop token state = do
    threadDelay 9999011 --Magic number here
    try <- getUpdate token $ offset state
    newState <- case try of
                    Right resp -> handleResponse state resp
                    Left err -> return $ state { returnedError = Just err }
    case returnedError newState of
        Just err -> errorAndContinue err (newState { returnedError = Nothing })
        Nothing -> updateLoop token newState
    where
        errorAndContinue err newState = do
                                            case err of
                                                InvalidResponse -> putStrLn "Error: Invalid response"
                                                (StatusCode code) -> putStrLn $ "Error code: " ++ show code
                                                (Other text) -> putStrLn $ "Error: " ++ show text
                                                (Exception e) -> putStrLn $ "Exception: " ++ show e
                                            updateLoop token newState

currentTime :: IO Int
currentTime = round <$> getPOSIXTime

processLoop :: State -> IO ()
processLoop state = do
    stack_val <- takeMVar (stacked state)
    case stack_val of
        [] -> do
                putMVar (stacked state) []
                threadDelay 674147 --Another one
                processLoop state
        (x:xs) -> do
                    putMVar (stacked state) xs
                    time <- currentTime
                    res <- proccessMessage x (curInterval state) time (admins state) (scheduled state)
                    let newState = case res of
                                        Just newAdmins -> state { admins = newAdmins }
                                        Nothing -> state
                    processLoop newState

scheduleLoop :: Token -> Schedule -> IO ()
scheduleLoop token schedule = do
    schedule_val <- takeMVar schedule
    case schedule_val of
        [] -> do
                putMVar schedule []
                continue
        (x:xs) -> do
                    time <- currentTime
                    if time >= time2Send x
                        then do
                                putMVar schedule xs
                                _ <- forkIO $ handleScheduled token (msg2Send x)
                                scheduleLoop token schedule
                        else do
                                putMVar schedule schedule_val
                                print $ time2Send x - time
                                continue
        where continue = do
                            threadDelay 1000000  --And another one
                            scheduleLoop token schedule
                    
handleScheduled :: Token -> Msg2Copy -> IO ()
handleScheduled token message = do
    _ <- changeName token (senderName message)
    resp <- copyMessage token message
    case resp of
        Right r -> case r ^? responseStatus . statusCode of
                        Just 200 -> return ()
                        Just code -> do
                                        putStrLn $ "Error code: " ++ show code
                        _ -> do
                                putStrLn "Error: Invalid response"
        Left err -> do
                    putStrLn $ "Error: " ++ show err

main :: IO ()
main = do
        putStrLn "Starting bot..."
        state <- initialState
        _ <- forkIO $ processLoop state
        _ <- forkIO $ scheduleLoop token (scheduled state)
        updateLoop token state
            where 
                token = "6294411428:AAEyIfDs5CVlgWHMQfePsr2OQ1eIuVLYaPU"
