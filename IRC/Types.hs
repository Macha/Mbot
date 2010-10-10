module IRC.Types where

import qualified Data.Map as Map
import System.IO

type RawIRCMessage = String
type ChannelName = String
type Username = String
type IRCHandle = Handle
type LogHandle = Handle
type CommandName = String
type PrivmsgCommandName = String
type ChannelList = Map.Map ChannelName ChannelWatch
type ServerName = String

data ChannelWatch = ChannelWatch {
                            connection :: IRCConnection,
                            channelName :: ChannelName }

data IRCConnection = IRCConnection { 
                                    serverName :: ServerName,
                                    network :: IRCHandle,
                                    logHandle :: LogHandle,
                                    channels :: ChannelList,
                                    nickUsed :: String }
