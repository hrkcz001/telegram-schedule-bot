module Timer (
    InitOpts(..),
    startTimer
) where

import Logic (Schedule, ScheduledMessage(..))
import Connection (Token, copyMessage, changeName)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Monad (void, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data InitOpts = InitOpts
    { initToken :: Token
    , initSchedule :: Schedule
    , initDefaultName :: Text
    , initDelay :: Int
    }

curTime :: IO Int
curTime = round <$> getPOSIXTime

startTimer :: InitOpts -> IO ()
startTimer opts = do
                    _ <- forkIO $ timerLoop (initToken opts) (initSchedule opts) (initDefaultName opts) (initDelay opts)
                    return ()

timerLoop :: Token -> Schedule -> Text -> Int -> IO ()
timerLoop token schedule defaultName delay = do
                        time <- curTime
                        schedule_val <- takeMVar schedule
                        case schedule_val of
                            [] -> do
                                    putMVar schedule []
                                    threadDelay $ delay * 1000000
                                    timerLoop token schedule defaultName delay
                            (x:xs) -> do
                                        if time >= time2Send x
                                            then do
                                                    when (name /= defaultName) $ 
                                                        void $ changeName token name
                                                    _ <- copyMessage token (msg2Send x)
                                                    putMVar schedule xs
                                                    timerLoop token schedule name delay
                                            else do
                                                    putMVar schedule schedule_val
                                                    threadDelay 1000000
                                                    timerLoop token schedule defaultName delay
                                            where
                                                name = fromMaybe defaultName (sender2Send x)
