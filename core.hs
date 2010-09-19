#!/usr/bin/runhaskell
module Main where
import IRC as IRC
import Data.List
import System.Environment
import System.IO

main = do 
    (server:port:nick:channel:xs) <- getArgs
    IRC.startIRC server (read port :: Int) nick channel

