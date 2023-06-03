{-# LANGUAGE OverloadedStrings #-}

module Connection
    (   getUpdate,
        copyMessage,
        changeName,
        Token
    ) where

import Update (Error(..), Msg2Copy(..))

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Wreq
import Data.ByteString.Lazy as ByteString
import Control.Exception (try)

type Token = String

getUpdate :: Token -> Int -> IO (Either Error (Response ByteString))
getUpdate token offset = do
                            resp <- try $ get url
                            case resp of
                                Right r -> return $ Right r
                                Left e -> return $ Left $ Exception e
                            where
                                url = "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ show offset

copyMessage :: Token -> Msg2Copy -> IO (Either Error (Response ByteString))
copyMessage token message = do
                                resp <- try $ post url formMsg
                                case resp of
                                    Right r -> return $ Right r
                                    Left e -> return $ Left $ Exception e
                                where
                                    url = "https://api.telegram.org/bot" ++ token ++ "/copyMessage"
                                    formMsg = [ "chat_id" := chatId message
                                              , "from_chat_id" := fromChatId message
                                              , "message_id" := messageId message
                                              ]

changeName :: Token -> Maybe Text -> IO ()
changeName token newName = do
                                _ <- post url formMsg
                                return ()
                                where
                                    url = "https://api.telegram.org/bot" ++ token ++ "/setMyName"
                                    formMsg = [ "name" := fromMaybe "Хтонь" newName
                                              ]

