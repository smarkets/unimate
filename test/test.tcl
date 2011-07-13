#!/usr/bin/tclsh
set sock [socket "127.0.0.1" 10101]
puts $sock $argv
flush $sock
close $sock
