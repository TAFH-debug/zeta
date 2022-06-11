{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    (
        pingpongExample
    ) where

import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (toLower, Text, isPrefixOf, pack, unpack, empty)
import qualified Data.Text.IO as TIO
import           Data.Maybe (fromMaybe)
import           Data.Either

import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import           Discord.Interactions
import Discord.Requests (UserRequest(GetUser))
import Control.Monad.IO.Class (MonadIO(liftIO))
import UnliftIO (MonadIO)

pingpongExample :: IO ()
pingpongExample = do
    tok <- TIO.readFile "auth-token.secret"
    userFacingError <- runDiscord $ def
             { 
                discordToken = tok, 
                discordOnEvent = eventHandler,
                discordOnStart = startHandler,
                discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }

    TIO.putStrLn userFacingError

startHandler :: DiscordHandler ()
startHandler = do
  let activity =
        def
          { 
            activityName = "~help",
            activityType = ActivityTypeGame
          }
  let opts =
        UpdateStatusOpts
          { 
            updateStatusOptsSince = Nothing,
            updateStatusOptsGame = Just activity,
            updateStatusOptsNewStatus = UpdateStatusOnline,
            updateStatusOptsAFK = False
          }
  sendCommand (UpdateStatus opts)

exampleSlashCommand :: Maybe CreateApplicationCommand
exampleSlashCommand =
  createChatInput
    "suggest"
    "here is a description"
    >>= \cac ->
      return $
        cac
          { createOptions =
              Just $
                OptionsValues
                  [ OptionValueString
                        "server"
                        "I shall not"
                        True
                        (Right [Choice "Discord" "Discord", Choice "Mindustry" "Mindustry"]),
                    OptionValueString 
                        "suggestion-title"
                        "Title of suggestion"
                        True
                        (Left False),
                    OptionValueString
                        "suggestion"
                        "Text of suggestion"
                        True
                        (Left False)
                  ]
          }

emptyUser :: User
emptyUser = User 1 "" Nothing Nothing True True Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

getAvatar :: MemberOrUser -> CreateEmbedImage
getAvatar (MemberOrUser (Right a)) = do
                let hash = fromMaybe "" $ userAvatar a
                    id = userId a
                CreateEmbedImageUrl $ pack $ "https://cdn.discordapp.com/avatars/"++show id++"/"++unpack hash++".png?size=512"

getAvatar (MemberOrUser (Left a)) = do
                let hash = fromMaybe "" $ memberAvatar a
                    id = userId $ fromMaybe emptyUser $ memberUser a
                    url = pack $ "https://cdn.discordapp.com/avatars/"++show id++"/"++unpack hash++".png?size=512"
                printLifted url
                CreateEmbedImageUrl url

printLifted :: MonadIO m => String -> m ()
printLifted a = liftIO (putStrLn a)

suggestIR :: OptionsData -> MemberOrUser -> InteractionResponse
suggestIR (OptionsDataValues [OptionDataValueString {optionDataValueString = s1}, 
                              OptionDataValueString {optionDataValueString = s2}, 
                              OptionDataValueString {optionDataValueString = s3} 
                              ]) user = do
                              let embeds = Just [ def { createEmbedTitle = pack ("Предложение для " ++ unpack (fromRight "" s1) ++ ":"),
                                    createEmbedDescription = "",
                                    createEmbedAuthorName = "",
                                    createEmbedImage = Nothing,
                                    createEmbedFields = [EmbedField (fromRight "" s2) (fromRight "" s3) (Just True),
                                                         EmbedField "Предложено пользователем:" "a" (Just True),
                                                         EmbedField "Общий результат голосов:" "0:white_check_mark:" (Just True)],
                                    createEmbedColor = Just DiscordColorLuminousVividPink,
                                    createEmbedAuthorIcon = Just $ getAvatar user
                                }]
                              let inter = InteractionResponseMessage Nothing Nothing embeds Nothing Nothing Nothing Nothing
                              InteractionResponseChannelMessage inter

                                

suggestIR _ _ =
  interactionResponseBasic
    "Something unexpected happened - the value was not what I expected!"

testserverid :: GuildId
testserverid = 833626513756258315

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> if not $ fromBot m
        then
            case messageContent m of
                "wow" -> void $ restCall (R.CreateReaction (messageChannelId m, messageId m) ":eyes:")
                "ping" -> void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
                "hello" -> void $ restCall (R.CreateMessage (messageChannelId m) "Hi!")
                _ -> return ()
        else
            return ()
    Ready _ _ _ _ _ _ (PartialApplication i _) -> do
        vs <-
            mapM
            (maybe (return (Left $ RestCallErrorCode 0 "" "")) (restCall . R.CreateGuildApplicationCommand i testserverid)) 
            [exampleSlashCommand]
        return ()
    InteractionCreate InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataChatInput {applicationCommandDataName = "suggest", optionsData = Just d, ..}, interactionUser = user, ..} ->
        void $
        restCall
            (R.CreateInteractionResponse interactionId interactionToken (suggestIR d user))
    _ -> return ()


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
