module Olai.Pipeline

import System

import Data.String

import Toolkit.Options.ArgParse

import Olai.Core

import Olai.Lexer
import Olai.Parser
import Olai.Check

import Olai.Terms
import Olai.Exec

import Olai.Options

%default total

showToks : (List (WithBounds Token)) -> List String
showToks = map (\(MkBounded t _ _) => show t)

export
pipeline : Opts -> Olai ()
pipeline opts
  = do fname <- embed
                  (Generic "File expected.")
                  (file opts)

       when (justLex opts)
         $ do toks <- lexFile fname
              putStrLn $ unwords (showToks toks)
              exitSuccess

       ast <- fromFile fname
       putStrLn "# Finished Parsing"

       tm <- progCheck ast
       putStrLn "# Finished Type Checking"

       when (justCheck opts)
         $ exitSuccess

       putStrLn "# Executing"
       putStrLn "```"
       v <- exec tm
       putStrLn "```"
       putStrLn "# Finished"
       pure ()

-- [ EOF ]
