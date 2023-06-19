{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Logic (
    InitOpts(..),
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

data Admins = Admins { adminsId :: [Int], adminsName :: [Text] }

data State = State  {   stateToken :: Token
                    ,   stateSchedule :: Schedule
                    ,   stateNextTime :: Int
                    ,   stateInterval :: Int
                    ,   statePassword :: Text
                    ,   stateAdmins :: Admins
                    ,   stateDestination :: Text
                    }

data InitOpts = InitOpts    {   initStack :: Stack
                            ,   initToken :: Token
                            ,   initInterval :: Int
                            ,   initDestination :: Text
                            ,   initPassword :: Text
                            ,   initAdmins :: [Text]
                            }

initState :: IO State
initState = do
                schedule <- newMVar []
                return $ State "" schedule 0 1 "" ( Admins [] [] ) ""

curTime :: IO Int
curTime = round <$> getPOSIXTime

process :: InitOpts -> IO Schedule
process InitOpts    { initStack = stack
                    , initToken = token
                    , initInterval = interval
                    , initDestination = destination
                    , initPassword = password
                    , initAdmins = admins }  
                    = do
                    emptyState <- initState
                    let state = emptyState  { stateDestination = destination
                                            , stateToken = token 
                                            , stateInterval = interval
                                            , statePassword = password
                                            , stateAdmins = Admins [] admins}
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
                                        (login -> True) -> return $ state { stateAdmins = appendAdmin admins msg }
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
                                        (first 5 -> "/wake") -> if isAdmin admins msg
                                                                then do
                                                                    schedule_val <- readMVar schedule
                                                                    time <- curTime
                                                                    return $ 
                                                                      if null schedule_val
                                                                        then state { stateNextTime = time }
                                                                        else state
                                                                else return state
                                        _ -> modifySchedule
                        _ -> modifySchedule
                        where 
                            token = stateToken state
                            interval = stateInterval state * 60
                            schedule = stateSchedule state
                            passwd = statePassword state
                            admins = stateAdmins state
                            first n = unpack . Data.Text.take n
                            login = (==) $ "/login " <> passwd
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

appendAdmin :: Admins -> Value -> Admins
appendAdmin admins msg = Admins 
                        (adminsId admins ++ appendId)
                        (adminsName admins ++ appendName)
    where   appendId = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> [adminId | adminId `notElem` adminsId admins]
                            Nothing -> []
            appendName = case senderLogin msg of
                            Just name -> [name | name `notElem` adminsName admins]
                            Nothing -> []

isAdmin :: Admins -> Value -> Bool
isAdmin admins msg = idMatch || nameMatch 
    where   idMatch   = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> adminId `elem` adminsId admins
                            Nothing -> False
            nameMatch = case senderLogin msg of
                            Just name -> name `elem` adminsName admins
                            Nothing -> False

senderName :: Value -> Maybe Text
senderName msg = (msg ^? key "from" . key "first_name" . _String) 
                    <> case msg ^? key "from" . key "last_name" . _String of
                            Just lastName -> Just $ " " <> lastName
                            Nothing -> Nothing

senderLogin :: Value -> Maybe Text
senderLogin msg = msg ^? key "from" . key "username" . _String

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
                                        let rawSleepSeconds = stateNextTime state - time
                                        let sleepSeconds = rawSleepSeconds `mod` 60
                                        let sleepMinutes = rawSleepSeconds `div` 60
                                        return $ pack $
                                            "Current Interval: " <> show (stateInterval state) <> 
                                                (if stateInterval state == 1
                                                    then " minute\n"
                                                    else " minutes\n")                                             
                                            <> (if null schedule_val
                                                then "No messages to send"
                                                <>   (if time < stateNextTime state
                                                        then "\nSleeping for: " <> 
                                                            if sleepMinutes > 0
                                                                then show sleepMinutes <> 
                                                                    if sleepMinutes == 1
                                                                        then " minute "
                                                                        else " minutes "
                                                                    <> show sleepSeconds <> 
                                                                        if sleepSeconds == 1
                                                                            then " second\n"
                                                                            else " seconds\n"
                                                                else show sleepSeconds <> 
                                                                    if sleepSeconds == 1
                                                                        then " second\n"
                                                                        else " seconds\n"
                                                        else "")
                                                else "Next message in: " <> 
                                                    (if nextMinutes schedule_val > 0
                                                        then show (nextMinutes schedule_val) <> 
                                                            if nextMinutes schedule_val == 1
                                                                then " minute "
                                                                else " minutes "
                                                            <> show (nextSeconds schedule_val) <> 
                                                                if nextSeconds schedule_val == 1
                                                                    then " second\n"
                                                                    else " seconds\n"
                                                        else show (nextSeconds schedule_val) <> 
                                                            if nextSeconds schedule_val == 1
                                                                then " second\n"
                                                                else " seconds\n")
                                                <> "Messages to send: " <> show (length schedule_val))
                                                    where rawNextSeconds s = max 0 (time2Send (head s) - time)
                                                          nextSeconds s = rawNextSeconds s `mod` 60
                                                          nextMinutes s = rawNextSeconds s `div` 60

responseNotAdmin :: Value -> IO Msg2Send
responseNotAdmin msg = return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    "You are not admin"
