module Olai.REPL.Load

import Toolkit.Data.Location

import Olai.Types
import Olai.Error.Pretty
import Olai.Core

import Olai.Raw.Types
import Olai.Raw.Types.View
import Olai.Raw.Exprs

import Olai.Raw.Stmts
import Olai.Raw.Funcs
import Olai.Raw.Roles
import Olai.Raw.Progs
import Olai.Raw.Progs.View

import Olai.Lexer
import Olai.Parser

import Olai.Check.Common
import Olai.Check.Roles
import Olai.Check.Types
import Olai.Check.Roles
import Olai.Check.Protocols
import Olai.Check.Exprs
import Olai.Check.Stmts
import Olai.Check.Funcs

import Olai.Terms

import Olai.REPL.State

%default total

check : {p     : Prog}
     -> {rs    : List Ty.Role}
     -> {ds,gs : List Ty.Base}
     -> (rho   : Context Ty.Role rs)
     -> (delta : Context Ty.Base ds)
     -> (gamma : Context Ty.Base gs)
     -> (state : State)
     -> (prog  : Prog p)
              -> Olai (Prog rs ds gs UNIT, State)

check rho delta gamma st (SeshDef fc ref s scope)
  = do (g ** tm) <- protocolCheck delta rho s

       (scope,st) <- check
                       rho
                       delta
                       gamma
                       ({protocols $= insert (get ref) (P rho tm)} st)
                       scope
       pure (DefSesh tm scope, st)

check rho delta gamma st (RoleDef fc ref scope)
  = do let rho = (extend rho (get ref) MkRole)
       (MkRole ** role) <- roleCheck rho (RoleRef ref)
       (scope, st) <- check
                       rho
                       delta
                       gamma
                       ({roles $= insert (get ref) (R (role))} st)
                       scope
       pure (DefRole scope, st)


check rho delta gamma st (TypeDef fc ref val scope)
  = do (ty ** tm) <- typeCheck delta val

       (scope, st) <- check
                        rho
                        (extend delta (get ref) ty)
                        gamma
                        ({ types $= insert (get ref) (T tm)} st)
                        scope
       pure (DefType tm scope, st)

check rho delta gamma st (FuncDef fc ref f scope)
  = do (FUNC as r ** f) <- funcCheck rho delta gamma f
         | (ty ** _) => throwAt fc (FunctionExpected ty)

       (scope, st) <- check
                        rho
                        delta
                        (extend gamma (get ref) (FUNC as r))
                        ({funcs $= insert (get ref) (F f)} st)
                        scope

       tyTm <- typeReflect delta (FUNC as r)
       pure (DefFunc tyTm f scope, st)


check rho delta gamma st (Main fc f)
  = do (FUNC Nil UNIT ** f) <- funcCheck rho delta gamma f
         | (ty ** _) => mismatchAt fc (FUNC Nil UNIT) ty

       pure (Main f, st)


checkProg : (r : Prog) -> Olai State
checkProg p
  = do (p,st) <- check Nil Nil Nil defaultState (view p)
       pure ({prog := Just p} st)

export
load : State -> String -> Olai State
load st fname
  = tryCatch (do ast <- fromFile fname
                 putStrLn "# Finished Parsing"

                 st <- checkProg ast
                 putStrLn "# Finished Type Checking"

                 pure st
              )
              (\err => do printLn err; pure st)
-- [ EOF ]
