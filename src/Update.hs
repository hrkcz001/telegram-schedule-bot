{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Update (
    handleResponse,
    initialState,
    State(..),
    Stack,
    Schedule,
    ScheduledMessage(..),
    Msg2Copy(..),
    Error(..),
) where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Concurrent.MVar
import Data.Vector as Vector
import Data.ByteString.Lazy as ByteString
import Network.Wreq
import Control.Exception (SomeException)

type Stack = MVar [Value]

data Msg2Copy = Msg2Copy {senderName :: Maybe Text,  chatId :: Text, fromChatId :: Int, messageId :: Int }
data ScheduledMessage = ScheduledMessage { msg2Send :: Msg2Copy, time2Send :: Int }
type Schedule = MVar [ScheduledMessage]

data State = State  {   offset :: Int 
                    ,   stacked :: Stack
                    ,   scheduled :: Schedule
                    ,   curInterval :: MVar Int
                    ,   admins :: [Int]
                    ,   returnedError :: Maybe Error 
                    }

data Error = InvalidResponse 
           | StatusCode Int
           | Exception SomeException
           | Other Text
         deriving (Show)

initialState :: IO State
initialState = do
                emptyStack <- newMVar []
                emptySchedule <- newMVar []
                interval <- newMVar 10
                return $ State 0 emptyStack emptySchedule interval [] Nothing

handleResponse :: State -> Response ByteString -> IO State
handleResponse state response = do
                                case response ^? responseStatus . statusCode of
                                    Just 200 -> case response ^? responseBody of
                                                    Just body -> handleBody state body
                                                    _ -> return $ state { returnedError = Just InvalidResponse }
                                    Just code -> return $ state { returnedError = Just (StatusCode code) }
                                    _ -> return $ state { returnedError = Just InvalidResponse }

handleBody :: State -> ByteString -> IO State
handleBody state body = case body ^? key "ok" of
    Just (Bool True) -> case body ^? key "result" of
                            Just result -> updateState state result
                            _ -> return $ state { returnedError = Just InvalidResponse }
    Just (Bool False) -> case body ^? key "description" of
                            Just (String s) -> return $ state { returnedError = Just (Other s) }
                            _ -> return $ state { returnedError = Just InvalidResponse }
    _ -> return $ state { returnedError = Just InvalidResponse }

updateState :: State -> Value -> IO State
updateState state result = do
                            newStack <- updateStack (stacked state) result
                            return $ State  (updateOffset (offset state) result)
                                            newStack
                                            (scheduled state)
                                            (curInterval state)
                                            (admins state)
                                            (returnedError state)

updateOffset :: Int -> Value -> Int
updateOffset curOffset (Array (Vector.null -> True)) = curOffset
updateOffset curOffset (Array vector) = case Vector.last vector ^? key "update_id" of
                                    Just (Number n) -> floor n + 1
                                    _ -> curOffset
updateOffset curOffset _ = curOffset

updateStack :: MVar [Value] -> Value -> IO (MVar [Value])
updateStack stack (Array vector) = do 
                                    curStack <- takeMVar stack
                                    putMVar stack $ curStack Prelude.++ Vector.toList vector
                                    return stack
updateStack stack _ = return stack
