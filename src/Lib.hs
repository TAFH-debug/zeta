{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
        pingpongExample
    ) where

import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

pingpongExample :: IO ()
pingpongExample = do
    userFacingError <- runDiscord $ def
             { discordToken = "ODIxMzE1OTk3MTUwNDc4MzM3.Gazlrr.d2Q2FjuyhKe9DuBkwPhfeX4uzfrWDTY7pm2r4k"
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }

    TIO.putStrLn userFacingError


eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (isPing m && not (fromBot m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10^6)
        void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
