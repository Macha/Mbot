module IRC (
startIRC
)
where

import Control.Monad
import Data.Bits
import Data.List
import qualified Data.Map as Map
import IRC.Log
import IRC.Parsers
import IRC.Types
import Network
import System.IO
import System.Posix
import Text.Printf


maxWaitForFirstPing = 10 

-- Command Creation
-- These take a command's parameter, and return a string that can be sent to the server.


createChannelMessage :: ChannelWatch -> String -> RawIRCMessage
createChannelMessage channel message = createPrivateMessage (channelName channel) message

createPrivateMessage :: String -> String -> RawIRCMessage
createPrivateMessage target message = unwords ["PRIVMSG", target, ':':message]


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


-- PM Handlers
-- These handle private message commands that need to be reacted to.

handlePrivmsg :: IRCHandle -> LogHandle -> RawIRCMessage -> Maybe ChannelWatch -> IO ()
handlePrivmsg nh fh message (Just channel)
    | privmsgIsHandled command_name = runHandler (Map.lookup command_name privmsgHandlers) channel nh
    | otherwise = logMessage fh (getCommandName message) message
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
sendVersion = sendTextToChannel "0.2-dev -- http://github.com/Macha/Mbot" 

sendInfo :: ChannelWatch -> IRCHandle -> IO ()
sendInfo channel = sendTextToChannel "Mbot Version 0.2-dev (c) Macha <macha@webicity.info> -- http://github.com/Macha/Mbot" channel

sendCommandList :: ChannelWatch -> IRCHandle -> IO ()
sendCommandList = sendTextToChannel "!info, !version, !commands"

-- General Handlers
-- These handle commands that aren't PMs. Also responsible for passing the PM messages off.

eval :: IRCConnection -> LogHandle -> RawIRCMessage -> ChannelName -> IO ()
eval con fh message channel
    | checkCommandName message "PRIVMSG" = handlePrivmsg (network con) fh message (Map.lookup channel (channels con))
    | commandHasLogger command = logMessage fh command message
    | commandIsIgnorable message = return ()
    | otherwise = hPutStrLn fh message
    where command = getCommandName message

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


-- Sets up the main IRC connection.
startIRC :: ServerName -> Int -> String -> ChannelName -> Maybe String -> IO () 
startIRC server port nick channel (Just password) = do
    connection <- getIRCConnection server port nick
    connection' <- doJoin connection channel
    putIRCLn (network connection') $ createPrivateMessage "NickServ" $ unwords ["IDENTIFY", password]
    listen connection' (logHandle connection') channel

startIRC server port nick channel Nothing = do
    connection <- getIRCConnection server port nick
    connection' <- doJoin connection channel
    listen connection' (logHandle connection') channel

getIRCConnection :: ServerName -> Int -> String -> IO IRCConnection
getIRCConnection server port nick = do
    file_handle <- openFile "mbot.log" AppendMode
    network_handle <- connectTo server (PortNumber (fromIntegral port))

    hSetBuffering network_handle NoBuffering
    hSetBuffering file_handle NoBuffering

    putIRCLn network_handle $ createNickMessage nick
    putIRCLn network_handle $ createUserMessage nick

    let connection = IRCConnection server network_handle file_handle Map.empty nick
    handleFirstPing connection -- Some networks include a pong reply as part of the join process
    return connection

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

