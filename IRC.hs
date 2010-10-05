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
type ChannelList = Map.Map ChannelName ChannelWatch

data ChannelWatch = ChannelWatch {
                            connection :: IRCConnection,
                            channelName :: ChannelName }

data IRCConnection = IRCConnection { 
                                    serverName :: String,
                                    network :: IRCHandle,
                                    logHandle :: LogHandle,
                                    channels :: ChannelList,
                                    nickUsed :: String }


maxWaitForFirstPing = 10 

-- Command Creation
-- These take a command's parameter, and return a string that can be sent to the server.

createChannelMessage :: ChannelWatch -> String -> RawIRCMessage
createChannelMessage channel message = unwords ["PRIVMSG", channelName channel, ":", message]

createNickMessage :: String -> RawIRCMessage
createNickMessage nick = unwords ["NICK", nick]

createUserMessage :: String -> RawIRCMessage
createUserMessage nick = "USER " ++ nick ++ " 0 * :" ++ nick

createPongMessage :: RawIRCMessage -> RawIRCMessage
createPongMessage ping = "PONG :" ++ getPingServer ping 

createJoinMessage :: ChannelName -> RawIRCMessage
createJoinMessage channel = unwords ["JOIN", channel]

createPartMessage :: ChannelName -> RawIRCMessage
createPartMessage channel = unwords ["PART", channel]

createQuitMessage = "QUIT" -- Mostly for consistency

-- Message Parsing 
-- These take a command, and extract relevant information from it.

getPingServer :: RawIRCMessage -> String
getPingServer = getMultiWordPortion

getMultiWordPortion :: RawIRCMessage -> String
getMultiWordPortion = concat . tail . split ':' . tail 

getHostMask :: RawIRCMessage -> Username
getHostMask = takeWhile(/= ' ') . tail . dropWhile (/= '!')
 
getUser :: RawIRCMessage -> Username
getUser = tail . takeWhile (/= '!')

-- Does not work for JOIN
getRoom :: RawIRCMessage -> ChannelName
-- RTL: Drop user info, drop space after user info, drop command, drop space after command, get first parameter (room where relevant)
getRoom = takeWhile (/= ' ') . tail . dropWhile (/= ' ') . tail . dropWhile (/= ' ')

-- Use getRoom for all other commands but join.
getJoinRoom :: RawIRCMessage -> ChannelName
getJoinRoom = getMultiWordPortion

getCommandName :: RawIRCMessage -> CommandName
getCommandName = takeWhile (/= ' ') . tail . dropWhile (/= ' ') 

getPrivmsgCommandName :: RawIRCMessage -> PrivmsgCommandName
getPrivmsgCommandName = takeWhile (/= ' ') . getMultiWordPortion

getArgumentByPosition :: RawIRCMessage -> Int -> CommandName
getArgumentByPosition message pos = words message !! pos

getUserAndMessage :: RawIRCMessage -> (Username, String)
getUserAndMessage x = (username, message) where
                username = getUser x 
                message = getMultiWordPortion x

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
formatPartForLog message = printf "%s left %s" (getUser message) ((words message) !! 3)

-- PM Handlers
-- These handle private message commands that need to be reacted to.

handlePrivmsg :: IRCHandle -> LogHandle -> RawIRCMessage -> Maybe ChannelWatch -> IO ()
handlePrivmsg nh fh message (Just channel)
    | privmsgIsHandled command_name = runHandler (Map.lookup command_name privmsgHandlers) channel nh
    | otherwise = logMessage fh (Just formatPrivmsgForLog) message
    where
        command_name = getPrivmsgCommandName message
handlePrivmsg nh fh message Nothing = return ()
   
runHandler :: Maybe (ChannelWatch -> IRCHandle -> IO ()) -> ChannelWatch -> IRCHandle -> IO ()
runHandler (Just f) = f

privmsgHandlers = Map.fromList [
    ("!version", sendVersion),
    ("!info", sendInfo),
    ("!commands", sendCommandList)]

privmsgIsHandled :: PrivmsgCommandName -> Bool
privmsgIsHandled command = Map.member command privmsgHandlers

sendVersion ::  ChannelWatch -> IRCHandle -> IO ()
sendVersion = sendTextToChannel "0.1" 

sendInfo :: ChannelWatch -> IRCHandle -> IO ()
sendInfo channel = sendTextToChannel "Version 0.1" channel

sendCommandList :: ChannelWatch -> IRCHandle -> IO ()
sendCommandList = sendTextToChannel "!info, !version, !commands"

-- General Handlers
-- These handle commands that aren't PMs. Also responsible for passing the PM messages off.

eval :: IRCConnection -> LogHandle -> RawIRCMessage -> ChannelName -> IO ()
eval con fh message channel
    | checkCommandName message "PRIVMSG" = handlePrivmsg (network con) fh message (Map.lookup channel (channels con))
    | commandHasFormatter command = logMessage fh (Map.lookup command commandFormatters) message
    | commandIsIgnorable message = return ()
    | otherwise = hPutStrLn fh message
    where command = getCommandName message

commandHasFormatter :: CommandName -> Bool
commandHasFormatter command = Map.member command commandFormatters

commandFormatters = Map.fromList [
    ("PRIVMSG", formatPrivmsgForLog),
    ("JOIN", formatJoinForLog),
    ("NICK", formatNickChangeForLog),
    ("PART", formatPartForLog),
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
    "376", -- End of MOTD
    -- Other
    "451"] -- Attempting to send messages before signing in right

commandIsIgnorable :: String -> Bool
commandIsIgnorable message = getCommandName message `elem` ignorableCommands

checkCommandName :: RawIRCMessage -> CommandName -> Bool
checkCommandName recieved command = command == getCommandName recieved 


-- Utility function
split :: Char -> String -> [String]
split delim [] = [""]
split delim (c:cs) 
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split delim cs

-- Sets up the main IRC connection.
startIRC :: String -> Int -> String -> String -> IO () 
startIRC server port nick channel = do
    file_handle <- openFile "mbot.log" AppendMode
    network_handle <- connectTo server (PortNumber (fromIntegral port))

    hSetBuffering network_handle NoBuffering
    hSetBuffering file_handle NoBuffering

    let connection = IRCConnection server network_handle file_handle Map.empty nick
    putIRCLn network_handle $ createNickMessage nick
    putIRCLn network_handle $ createUserMessage nick

    handleFirstPing connection -- Some networks include a pong reply as part of the join process
    connection' <- doJoin connection channel
    listen connection' file_handle channel

-- Input methods
-- These actually get data from the server.

handleFirstPing :: IRCConnection -> IO ()
handleFirstPing con = handleFirstPing' (network con) 0 where
    handleFirstPing' :: IRCHandle -> Int -> IO ()
    handleFirstPing' nh counter = do
        recieved <- recieveMessage nh
        handlePing nh recieved (return ()) $
            if counter < maxWaitForFirstPing
                then handleFirstPing' nh (counter + 1)
                else return () -- Presume network doesn't need PONG, prevent excessive delays in startup

handlePing :: IRCHandle -> String -> IO () -> IO () -> IO ()
handlePing nh message pingfunc elsefunc
    | "PING" `isPrefixOf` message = putIRCLn nh (createPongMessage message) >> pingfunc 
    | otherwise = elsefunc

listen :: IRCConnection -> LogHandle -> String -> IO ()
listen con fh channel = forever $ do
    recieved <- recieveMessage (network con)
    handlePing (network con) recieved (return ()) (eval con fh recieved channel)

recieveMessage :: IRCHandle -> IO String
recieveMessage nh = hGetLine nh >>= return . init

putIRCLn :: IRCHandle -> String -> IO ()
putIRCLn nh str = hPrintf nh (str ++ "\r\n")

-- Output metods
-- These actually send data to the server.
doJoin :: IRCConnection -> ChannelName -> IO IRCConnection
doJoin con channel = do
    let message = createJoinMessage channel
    putIRCLn (network con) message
    let channel_watch = ChannelWatch con channel
    let new_channels = Map.insert channel channel_watch $ channels con
    let new_con = IRCConnection (serverName con) (network con) (logHandle con) new_channels (nickUsed con)
    return new_con
    

sendTextToChannel :: String -> ChannelWatch -> IRCHandle -> IO ()
sendTextToChannel message channel handle = putIRCLn handle $ createChannelMessage channel message

logMessage :: LogHandle -> Maybe (RawIRCMessage -> String) -> RawIRCMessage -> IO ()
logMessage fh (Just formatf) = hPutStrLn fh . formatf 
