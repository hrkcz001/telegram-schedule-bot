{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Logic (
    proccessMessage
) where

import Update (Msg2Copy(..), ScheduledMessage(..), Schedule)

import Data.Text (Text, unpack, take, drop, strip)
import Data.Text.Read (decimal)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent.MVar

proccessMessage :: Value -> MVar Int -> Int -> [Int] -> Schedule -> IO (Maybe [Int])
proccessMessage message interval time admins schedule = do
    case message ^? key "message" of
        Just msg -> case msg ^? key "text" . _String of
                        Just text -> case text of
                                        "7Qe2bZJ1LG" -> return $ appendAdmin admins msg
                                        (unpack . Data.Text.take 9 -> "/interval") -> 
                                                if isAdmin admins msg
                                                    then do
                                                        let newInterval = decimal $ strip $ Data.Text.drop 9 text
                                                        case newInterval of
                                                            Right (val, _) -> do
                                                                                _ <- takeMVar interval
                                                                                putMVar interval (val * 60)
                                                                                return Nothing
                                                            Left _ -> return Nothing
                                                    else return Nothing
                                        _ -> modifySchedule
                        _ -> modifySchedule
                        where modifySchedule = if isAdmin admins msg
                                                    then do
                                                        schedule_val <- takeMVar schedule
                                                        interval_val <- readMVar interval
                                                        let lastTime = if null schedule_val 
                                                                            then time 
                                                                            else time2Send $ last schedule_val
                                                        putMVar schedule (schedule_val ++ [ScheduledMessage 
                                                                                    (formCopyRequest msg) 
                                                                                    (interval_val + lastTime)])
                                                        return Nothing
                                                    else return Nothing
        _ -> return Nothing

formCopyRequest :: Value -> Msg2Copy
formCopyRequest message = Msg2Copy
                            ("@xuivragam" :: Text)
                            (message ^?! key "chat" . key "id" . _Integral)
                            (message ^?! key "message_id" . _Integral)

appendAdmin :: [Int] -> Value -> Maybe [Int]
appendAdmin admins msg = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> if adminId `elem` admins
                                            then Nothing
                                            else Just $ admins ++ [adminId]
                            Nothing -> Nothing

isAdmin :: [Int] -> Value -> Bool
isAdmin admins msg = case msg ^? key "chat" . key "id" . _Integral of
                        Just adminId -> adminId `elem` admins
                        Nothing -> False
