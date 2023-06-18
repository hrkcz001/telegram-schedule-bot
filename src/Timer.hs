module Timer (startTimer) where

import Logic (Schedule, ScheduledMessage(..))
import Connection (Token, copyMessage, changeName)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

curTime :: IO Int
curTime = round <$> getPOSIXTime

startTimer :: Token -> Schedule -> Text -> IO ()
startTimer token schedule defaultName = do
                        _ <- forkIO $ timerLoop token schedule defaultName
                        return ()

timerLoop :: Token -> Schedule -> Text -> IO ()
timerLoop token schedule defaultName = do
                        time <- curTime
                        schedule_val <- takeMVar schedule
                        case schedule_val of
                            [] -> do
                                    putMVar schedule []
                                    threadDelay 60000000 -- 1 minute
                                    timerLoop token schedule defaultName
                            (x:xs) -> do
                                        if time >= time2Send x
                                            then do
                                                    _ <- changeName token name
                                                    _ <- copyMessage token (msg2Send x)
                                                    _ <- changeName token defaultName
                                                    putMVar schedule xs
                                                    timerLoop token schedule defaultName
                                            else do
                                                    putMVar schedule schedule_val
                                                    threadDelay 1000000
                                                    timerLoop token schedule defaultName
                                            where
                                                name = fromMaybe defaultName (sender2Send x)
