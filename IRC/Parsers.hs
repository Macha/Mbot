module IRC.Parsers where

import IRC.Types
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

-- Utility function
split :: Char -> String -> [String]
split delim [] = [""]
split delim (c:cs) 
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split delim cs


