{-# LANGUAGE OverloadedStrings #-}

module Connection
    (   getUpdate,
        copyMessage,
        sendMessage,
        changeName,
        Token,
        Error(..),
        Msg2Copy(..),
        Msg2Send(..)
    ) where

import Data.Text (Text)
import Network.Wreq
import Data.ByteString.Lazy as ByteString
import Control.Exception (try, SomeException)

type Token = String

data Msg2Copy = Msg2Copy { senderName2Copy :: Maybe Text,  chatId2Copy :: Text, fromChatId2Copy :: Int, messageId2Copy :: Int }
data Msg2Send = Msg2Send { chatId2Send :: Int, reply2Msg2Send :: Maybe Int, text2Send :: Text}

data Error = InvalidResponse 
           | StatusCode Int
           | Exception SomeException
           | Other Text
         deriving (Show)

--shit happens
tryTo :: IO a -> IO (Either Error a)
tryTo action = do
                res <- try action
                case res of
                    Right r -> return $ Right r
                    Left e -> return $ Left $ Exception e

getUpdate :: Token -> Int -> Int -> IO (Either Error (Response ByteString))
getUpdate token timeout offset = do
                            tryTo $ get url
                            where
                                url =  "https://api.telegram.org/bot" ++ token 
                                    ++ "/getUpdates?offset=" ++ show offset 
                                    ++ "&timeout=" ++ show timeout

copyMessage :: Token -> Msg2Copy -> IO (Either Error (Response ByteString))
copyMessage token message = do
                            tryTo $ post url formMsg
                            where
                                url = "https://api.telegram.org/bot" ++ token ++ "/copyMessage"
                                formMsg = [ "chat_id" := chatId2Copy message
                                          , "from_chat_id" := fromChatId2Copy message
                                          , "message_id" := messageId2Copy message
                                          ]

sendMessage :: Token -> Msg2Send -> IO (Either Error (Response ByteString))
sendMessage token message = do
                            tryTo $ post url formMsg
                            where
                                url = "https://api.telegram.org/bot" ++ token ++ "/sendMessage"
                                formMsg = [ "chat_id" := chatId2Send message
                                          , "text" := text2Send message
                                          ] ++ case reply2Msg2Send message of
                                                Nothing -> []
                                                Just msgId -> [ "reply_to_message_id" := msgId ]

changeName :: Token -> Text -> IO (Either Error (Response ByteString))
changeName token newName =  do
                            tryTo $ post url formMsg
                            where
                                url = "https://api.telegram.org/bot" ++ token ++ "/setMyName"
                                formMsg = [ "name" := newName ]
