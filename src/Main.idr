module Main

import System
import Olai.Core

import Olai.Pipeline
import Olai.REPL
import Olai.Options


mainRug : Olai ()
mainRug
  = do opts <- getOpts

       if (launchREPL opts)
         then repl
         else pipeline opts

       exitSuccess


main : IO ()
main
  = do Olai.run mainRug

-- [ EOF ]
