{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Logic (
    Schedule,
    ScheduledMessage(..),
    process
) where

import Update (Stack, popUpdate, putError)
import Connection (Token, Msg2Copy(..), Msg2Send(..), sendMessage)

import Data.Text (Text, pack, unpack, take, drop, strip)
import Data.Text.Read (decimal)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX (getPOSIXTime)

data ScheduledMessage = ScheduledMessage { sender2Send :: Maybe Text, msg2Send :: Msg2Copy, time2Send :: Int }
type Schedule = MVar [ScheduledMessage]

data State = State  {   stateToken :: Token
                    ,   stateSchedule :: Schedule
                    ,   stateNextTime :: Int
                    ,   stateInterval :: Int
                    ,   stateAdmins :: [Int]
                    ,   stateDestination :: Text
                    }

initState :: IO State
initState = do
                schedule <- newMVar []
                return $ State "" schedule 0 1 [] ""

curTime :: IO Int
curTime = round <$> getPOSIXTime

process :: Token -> Stack -> Text -> IO Schedule
process token stack destination = do
                    emptyState <- initState
                    let state = emptyState { stateDestination = destination, stateToken = token }
                    _ <- forkIO $ processLoop stack state
                    return $ stateSchedule state

processLoop :: Stack -> State -> IO ()
processLoop stack state = do
                    update <- popUpdate stack
                    case update of
                        Nothing -> do
                                    threadDelay 1000000
                                    processLoop stack state
                        Just val -> do
                                    newState <- processMessage stack state val
                                    processLoop stack newState

processMessage :: Stack -> State -> Value -> IO State
processMessage stack state message = do
    case message ^? key "message" of
        Just msg -> case msg ^? key "text" . _String of
                        Just text -> case text of
                                        (first 1 -> "!") -> return state
                                        "/login 7Qe2bZJ1LG" -> return $ state { stateAdmins = appendAdmin admins msg }
                                        (first 9 -> "/interval") ->
                                                if isAdmin admins msg
                                                    then
                                                        case newInterval of
                                                            Right (val, _) -> return $ state { stateInterval = val }
                                                            Left _ -> return state
                                                    else return state
                                                    where newInterval = decimal $ strip $ Data.Text.drop 9 text
                                        (first 7 -> "/status") ->
                                                if isAdmin admins msg
                                                    then do
                                                        time <- curTime
                                                        response <- formStatusResponse msg time state schedule
                                                        result <- sendMessage token response
                                                        case result of
                                                            Right _ -> return state
                                                            Left e -> do
                                                                        putError stack e
                                                                        return state
                                                    else do
                                                        response <- responseNotAdmin msg
                                                        result <- sendMessage token response
                                                        case result of
                                                            Right _ -> return state
                                                            Left e -> do
                                                                        putError stack e
                                                                        return state
                                        _ -> modifySchedule
                        _ -> modifySchedule
                        where 
                            token = stateToken state
                            interval = stateInterval state * 60
                            schedule = stateSchedule state
                            admins = stateAdmins state
                            first n = unpack . Data.Text.take n
                            modifySchedule = if isAdmin admins msg
                                                    then do
                                                        time <- curTime
                                                        let nextTime = max time $ stateNextTime state
                                                        schedule_val <- takeMVar schedule
                                                        putMVar schedule (schedule_val ++ [ScheduledMessage
                                                                                    (senderName msg)
                                                                                    (formCopyRequest (stateDestination state) msg)
                                                                                    nextTime])
                                                        return state { stateNextTime = nextTime + interval }
                                                    else return state
        _ -> return state

formCopyRequest :: Text -> Value -> Msg2Copy
formCopyRequest destination message = Msg2Copy
                            name 
                            destination
                            (message ^?! key "chat" . key "id" . _Integral)
                            (message ^?! key "message_id" . _Integral)
                                where name = (message ^? key "from" . key "first_name" . _String) 
                                                <> case message ^? key "from" . key "last_name" . _String of
                                                        Just lastName -> Just $ " " <> lastName
                                                        Nothing -> Nothing

appendAdmin :: [Int] -> Value -> [Int]
appendAdmin admins msg = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> if adminId `elem` admins
                                            then admins
                                            else admins ++ [adminId]
                            Nothing -> admins

isAdmin :: [Int] -> Value -> Bool
isAdmin admins msg = case msg ^? key "chat" . key "id" . _Integral of
                        Just adminId -> adminId `elem` admins
                        Nothing -> False

senderName :: Value -> Maybe Text
senderName msg = (msg ^? key "from" . key "first_name" . _String) 
                    <> case msg ^? key "from" . key "last_name" . _String of
                            Just lastName -> Just $ " " <> lastName
                            Nothing -> Nothing

formStatusResponse :: Value -> Int -> State -> Schedule -> IO Msg2Send
formStatusResponse msg time state schedule = do 
                            text <- formStatusText time state schedule
                            return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    text

formStatusText :: Int -> State -> Schedule -> IO Text
formStatusText time state schedule = do 
                                        schedule_val <- readMVar schedule
                                        return $ pack $
                                            "Interval: " <> show (stateInterval state) <> " minutes\n" <>
                                            if null schedule_val
                                                then "No messages to send"
                                                else "Next message in: " <> show (max 0 (time2Send (last schedule_val) - time))
                                                <>   " seconds\n" 
                                                <>   "Messages to send: " <> show (length schedule_val)

responseNotAdmin :: Value -> IO Msg2Send
responseNotAdmin msg = return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    "You are not admin"
