module IRC (
createChannelMessage,
createStartMessage,
createJoinMessage,
createPartMessage,
getPingServer,
createPongMessage,
getMultiWordPortion,
split,
startIRC
)
where

import Data.Bits
import Data.List
import qualified Data.Map as Map
import Network
import System.IO
import Text.Printf

type RawIRCMessage = String
type ChannelName = String
type Username = String
type IRCHandle = Handle
type LogHandle = Handle
type CommandName = String

createChannelMessage :: ChannelName -> String -> RawIRCMessage
createChannelMessage channel message = "PRIVMSG "  ++ channel ++ " :" ++ message 

createStartMessage :: Username -> [RawIRCMessage]
createStartMessage username = ["NICK " ++ username, "USER " ++ username ++ "8" ++ "* : Haskell bot"]

createPongMessage :: RawIRCMessage -> RawIRCMessage
createPongMessage ping = "PONG :" ++ getPingServer ping 

createJoinMessage :: ChannelName -> RawIRCMessage
createJoinMessage channel = "JOIN " ++ channel

createPartMessage :: ChannelName -> RawIRCMessage
createPartMessage channel = "PART " ++ channel

getPingServer :: RawIRCMessage -> String
getPingServer = getMultiWordPortion

getMultiWordPortion :: String -> String
getMultiWordPortion = concat . tail . split ':' . drop 1
 
getUser :: RawIRCMessage -> Username
getUser = drop 1 . takeWhile (/= '!')

getCommandName :: RawIRCMessage -> CommandName
getCommandName = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ') 

getUserAndMessage :: RawIRCMessage -> (Username, String)
getUserAndMessage x = (username, message) where
                username = getUser x 
                message = getMultiWordPortion x

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

split :: Char -> String -> [String]
split delim [] = [""]
split delim (c:cs) 
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split delim cs

startIRC :: String -> Int -> String -> String -> IO () 
startIRC server port nick channel = do
    file_handle <- openFile "mbot.log" AppendMode
    network_handle <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering network_handle NoBuffering
    hSetBuffering file_handle NoBuffering
    write network_handle "NICK" nick
    write network_handle "USER" $ nick ++ " 0 * :Haskell Bot"
    write network_handle "JOIN" channel
    listen network_handle file_handle

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t

listen :: IRCHandle -> LogHandle -> IO ()
listen nh fh = forever $ do
       t <- hGetLine nh 
       let s = init t
       if ping s then pong s else eval nh fh s
       putStrLn s
   where
       forever a = a >> forever a 
       ping x = "PING" `isPrefixOf` x
       pong x = hPrintf nh $ (createPongMessage x) ++ "\r\n" 

eval :: IRCHandle -> LogHandle -> String -> IO ()
eval nh fh message
    | commandHasFormatter command = logMessage fh (Map.lookup command commandFormatters) message
    -- | checkCommandName message "PRIVMSG" = logMessage fh formatPrivmsgForLog message
    -- | checkCommandName message "JOIN" = logMessage fh formatJoinForLog message
    -- | checkCommandName message "QUIT" = logMessage fh formatQuitForLog message
    -- | checkCommandName message "NOTICE" = logMessage fh formatNoticeForLog message
    -- | checkCommandName message "353" = logMessage fh formatNamesListForLog message
    -- | checkCommandName message "332" = logMessage fh formatTopicForLog message
    | commandIsIgnorable message = return ()
    | otherwise = hPutStrLn fh message
    where command = getCommandName message

commandHasFormatter :: CommandName -> Bool
commandHasFormatter command = Map.member command commandFormatters

commandFormatters = Map.fromList [
    ("PRIVMSG", formatPrivmsgForLog),
    ("JOIN", formatJoinForLog),
    ("QUIT", formatQuitForLog),
    ("NOTICE", formatNoticeForLog),
    ("353", formatNamesListForLog),
    ("332", formatTopicForLog)]

ignorableCommands = [
    -- Commands we don't need yet
    "MODE", 
    -- Welcome messages
    "001", -- Welcome
    "002", -- Host info
    "003", -- Host uptime
    "004", -- Version info?
    "005", -- Server config
    -- Counts (seriously, why does it send these to every client?) Useful - http://mirc.net/raws/ 
    "250", -- connection count
    "251", -- user count
    "252", -- op count
    "253", -- unknown connections count
    "254", -- channel count
    "255", -- client count
    "265", -- local user count
    "266", -- global user count
    -- Join related
    "333", -- the user who set the topic
    "366", -- End of names
    -- MOTD related
    "372", -- MOTD
    "375", -- MOTD start
    "376"] -- End of MOTD

commandIsIgnorable :: String -> Bool
commandIsIgnorable message = getCommandName message `elem` ignorableCommands

checkCommandName :: RawIRCMessage -> CommandName -> Bool
checkCommandName recieved command = command == getCommandName recieved 

logMessage :: LogHandle -> Maybe (RawIRCMessage -> String) -> RawIRCMessage -> IO ()
logMessage fh (Just formatf) = hPutStrLn fh . formatf 
