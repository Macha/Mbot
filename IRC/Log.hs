module IRC.Log (
logMessage,
commandHasLogger,
)
where

import Data.List
import qualified Data.Map as Map
import IRC.Parsers
import IRC.Types
import Network
import System.IO
import Text.Printf

commandHasLogger :: CommandName -> Bool
commandHasLogger command = Map.member command commandFormatters

commandFormatters = Map.fromList [
    ("PRIVMSG", formatPrivmsgForLog),
    ("JOIN", formatJoinForLog),
    ("NICK", formatNickChangeForLog),
    ("PART", formatPartForLog),
    ("QUIT", formatQuitForLog),
    ("NOTICE", formatNoticeForLog),
    ("353", formatNamesListForLog),
    ("332", formatTopicForLog)]


logMessage :: LogHandle -> CommandName -> RawIRCMessage -> IO ()
logMessage fh command message = writeLogMessage fh (Map.lookup command commandFormatters) message

writeLogMessage :: LogHandle -> Maybe (RawIRCMessage -> String) -> RawIRCMessage -> IO ()
writeLogMessage fh (Just formatf) = hPutStrLn fh . formatf 

-- Format Messages For Logs
-- These take a recieved message, and return a format suitable for writing to the log file.

formatFromUserForLog :: RawIRCMessage -> String -> String
formatFromUserForLog format_string x = printf format_string username message where  
                details = getUserAndMessage x
                username = fst details
                message = snd details

formatPrivmsgForLog :: RawIRCMessage -> String
formatPrivmsgForLog = formatFromUserForLog "<%s> %s"

formatNoticeForLog :: RawIRCMessage -> String
formatNoticeForLog = printf "Notice: %s" . getMultiWordPortion

formatQuitForLog :: RawIRCMessage -> String
formatQuitForLog = formatFromUserForLog "%s quit (%s)"

formatJoinForLog :: RawIRCMessage -> String
formatJoinForLog = formatFromUserForLog "%s joined %s"

formatTopicForLog :: RawIRCMessage -> String
formatTopicForLog = printf "Topic: %s" . getMultiWordPortion

formatNamesListForLog :: RawIRCMessage -> String
formatNamesListForLog = printf "Users in room: %s" . getMultiWordPortion

formatNickChangeForLog :: RawIRCMessage -> String
formatNickChangeForLog = formatFromUserForLog "%s changed nick to %s"

formatPartForLog :: RawIRCMessage -> String
formatPartForLog message = printf "%s left %s" (getUser message) ((words message) !! 2)

