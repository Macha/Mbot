#!/usr/bin/runhaskell
module Mbot.Core where
import Mbot.IRC as IRC

server = "irc.freenode.org"
port = 6667
nick = "hbottest"
channel = "#hbot-channel"

main = IRC.startIRC server port nick channel 
