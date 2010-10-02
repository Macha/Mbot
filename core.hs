#!/usr/bin/runhaskell
module Main where
import IRC as IRC
import Data.List
import System.Environment
import System.IO

main = do 
    args <- getArgs
    handleArgs args


handleArgs :: [String] -> IO ()
handleArgs (server:port:nick:channel:xs) = IRC.startIRC server (read port :: Int) nick channel
handleArgs (server:nick:channel:xs) = IRC.startIRC server 6667 nick channel
