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
handleArgs (server:nick:channel:[]) = IRC.startIRC server 6667 nick channel
handleArgs (server:port:nick:channel:[]) = IRC.startIRC server (read port :: Int) nick channel
handleArgs _ = putStrLn "Wrong arguments"
