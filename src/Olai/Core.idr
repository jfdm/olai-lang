||| The Core Computation Context.
|||
||| Module    : Core.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
||| `TheRug` is defined in the toolkit. Here we establish the synonyms
||| required for Olai.
|||
module Olai.Core

import        System

import        Data.String

import public Toolkit.TheRug
import        Toolkit.System

import public Olai.Bootstrap
import public Olai.Error
import        Olai.Error.Pretty

%default total

public export
%inline
Olai : Type -> Type
Olai = TheRug Olai.Error

namespace Olai

  %inline
  whenErr : (msg : Olai.Error)
                -> IO ()
  whenErr err
    = do putStrLn (show err)
         exitWith (ExitFailure 1)

  %inline
  whenOK : a -> IO ()
  whenOK _ = pure ()

  export
  run : (prog : Olai a)
             -> IO ()
  run = run whenErr whenOK

-- [ EOF ]
