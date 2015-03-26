{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Handler.Chat where

import Import
import Yesod.WebSockets
import Data.Text (Text)
import qualified Network.WebSockets as WS
import qualified Data.Text.Lazy as TL
import Control.Concurrent (threadDelay)

getChatR :: Handler Value
getChatR = do
    app <- getYesod
    let chan = appChan app
    webSockets $ do
        sendTextData ("Welcome to the chat server, please enter your name." :: Text)
        name <- receiveData
        sendTextData $ "Welcome, " <> name
        rChan <- atomically (dupTChan chan)
        race_
            (forever $ atomically (readTChan rChan) >>= sendTextData)
            (sourceWS $$ mapM_C (\msg ->
                atomically $ writeTChan chan $ name <> ": " <> msg))
    returnJson ()


timeSource :: MonadIO m => Source m TL.Text
timeSource = forever $ do
    now <- liftIO getCurrentTime
    yield $ TL.pack $ show now
    liftIO $ threadDelay 5000000

--chatApp :: WebSocketsT Handler ()
--chatApp = do
--    sendTextData ("Welcome to the chat server, please enter your name." :: Text)
--    name <- receiveData
--    sendTextData $ name
--    App writeChan <- getYesod
--    readChan <- atomically $ do
--        writeTChan writeChan $ name ++ " has joined the chat"
--        dupTChan writeChan
--    race_
--        (forever $ atomically (readTChan readChan) >>= sendTextData)
--        (sourceWS $$ mapM_C (\msg ->
--            atomically $ writeTChan writeChan $ name ++ ": " ++ msg))