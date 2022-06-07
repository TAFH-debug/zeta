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
             { discordToken = ""
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }

    TIO.putStrLn userFacingError


eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> if not $ fromBot m 
        then
            case messageContent m of
                "ping" -> void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
                "hello" -> void $ restCall (R.CreateMessage (messageChannelId m) "Hi!")
        else
            return ()
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
