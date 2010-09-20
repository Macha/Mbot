module IRC (
startIRC
)
where

import Data.Bits
import Data.List
import Control.Monad
import qualified Data.Map as Map
import Network
import System.IO
import System.Posix
import Text.Printf

type RawIRCMessage = String
type ChannelName = String
type Username = String
type IRCHandle = Handle
type LogHandle = Handle
type CommandName = String
type PrivmsgCommandName = String

createChannelMessage :: ChannelName -> String -> RawIRCMessage
createChannelMessage channel message = unwords ["PRIVMSG", channel, ":", message]

createPongMessage :: RawIRCMessage -> RawIRCMessage
createPongMessage ping = "PONG :" ++ getPingServer ping 

createJoinMessage :: ChannelName -> RawIRCMessage
createJoinMessage channel = unwords ["JOIN", channel]

createPartMessage :: ChannelName -> RawIRCMessage
createPartMessage channel = unwords ["PART", channel]

getPingServer :: RawIRCMessage -> String
getPingServer = getMultiWordPortion

getMultiWordPortion :: String -> String
getMultiWordPortion = concat . tail . split ':' . drop 1
 
getUser :: RawIRCMessage -> Username
getUser = drop 1 . takeWhile (/= '!')

getCommandName :: RawIRCMessage -> CommandName
getCommandName = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ') 

getPrivmsgCommandName :: RawIRCMessage -> PrivmsgCommandName
getPrivmsgCommandName = takeWhile (/= ' ') . getMultiWordPortion


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

handlePrivmsg :: IRCHandle -> LogHandle -> RawIRCMessage -> ChannelName -> IO ()
handlePrivmsg nh fh message channel 
    | privmsgIsHandled command_name = runHandler (Map.lookup command_name privmsgHandlers) channel nh
    | otherwise = logMessage fh (Just formatPrivmsgForLog) message
    where
        command_name = getPrivmsgCommandName message
   
runHandler :: Maybe (ChannelName -> IRCHandle -> IO ()) -> ChannelName -> IRCHandle -> IO ()
runHandler (Just f) = f

privmsgHandlers = Map.fromList [
    ("!version", sendVersion),
    ("!info", sendInfo),
    ("!commands", sendCommandList)]

privmsgIsHandled :: PrivmsgCommandName -> Bool
privmsgIsHandled command = Map.member command privmsgHandlers

sendTextToChannel :: String -> ChannelName -> IRCHandle -> IO ()
sendTextToChannel message channel handle = hPutStrLn handle $ createChannelMessage channel message

sendVersion ::  ChannelName -> IRCHandle -> IO ()
sendVersion = sendTextToChannel "0.1" 

sendInfo :: ChannelName -> IRCHandle -> IO ()
sendInfo channel = sendTextToChannel "Version 0.1" channel

sendCommandList :: ChannelName -> IRCHandle -> IO ()
sendCommandList = sendTextToChannel "!info, !version, !commands"

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
    write network_handle "USER" $ nick ++ " 0 * :" ++ nick
    handleFirstPing network_handle channel 0 -- Some networks include a pong reply as part of the join process
    -- write network_handle "JOIN" channel
    listen network_handle file_handle channel

handleFirstPing :: IRCHandle -> ChannelName -> Int -> IO ()
handleFirstPing nh channel counter = do
    t <- hGetLine nh
    let recieved = init t
    if ping recieved
        then pong recieved 
        else if counter < 10
            then handleFirstPing nh channel (counter + 1)
            else write nh "JOIN" channel -- Presume network doesn't need PONG, prevent excessive delays in startup
    where
        ping x = "PING" `isPrefixOf` x
        pong x = do 
            hPrintf nh $ (createPongMessage x) ++ "\r\n" 
            write nh "JOIN" channel

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf "> %s %s\n" s t

listen :: IRCHandle -> LogHandle -> String -> IO ()
listen nh fh channel = forever $ do
    t <- hGetLine nh 
    let recieved = init t
    if ping recieved then pong recieved else eval nh fh recieved channel
    putStrLn recieved
    where
       ping x = "PING" `isPrefixOf` x
       pong x = hPrintf nh $ (createPongMessage x) ++ "\r\n" 

eval :: IRCHandle -> LogHandle -> RawIRCMessage -> ChannelName -> IO ()
eval nh fh message channel
    | checkCommandName message "PRIVMSG" = handlePrivmsg nh fh message channel
    | commandHasFormatter command = logMessage fh (Map.lookup command commandFormatters) message
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
