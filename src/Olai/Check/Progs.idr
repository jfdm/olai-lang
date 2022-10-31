||| Type-checker for Programs.
|||
||| Module    : Types.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Check.Progs

import Toolkit.Data.Location

import Olai.Types
import Olai.Core

import Olai.Raw.Types
import Olai.Raw.Types.View
import Olai.Raw.Exprs

import Olai.Raw.Stmts
import Olai.Raw.Funcs
import Olai.Raw.Roles
import Olai.Raw.Progs
import Olai.Raw.Progs.View

import Olai.Check.Common

import Olai.Check.Roles
import Olai.Check.Types
import Olai.Check.Roles
import Olai.Check.Protocols
import Olai.Check.Exprs
import Olai.Check.Stmts
import Olai.Check.Funcs

import Olai.Terms.Vars
import Olai.Terms.Roles
import Olai.Terms.Protocols
import Olai.Terms.Types
import Olai.Terms.Exprs
import Olai.Terms.Stmts
import Olai.Terms.Funcs
import Olai.Terms.Progs

%default total


check : {p     : Prog}
     -> {rs    : List Ty.Role}
     -> {ds,gs : List Ty.Base}
     -> (rho   : Context Ty.Role rs)
     -> (delta : Context Ty.Base ds)
     -> (gamma : Context Ty.Base gs)
     -> (prog  : Prog p)
              -> Olai (Prog rs ds gs UNIT)

check rho delta gamma (SeshDef fc ref s scope)
  = do (g ** tm) <- protocolCheck delta rho s
       scope <- check
                  rho
                  delta
                  gamma
                  scope
       pure (DefSesh tm scope)

check rho delta gamma (RoleDef fc ref scope)
  = do let rho = (extend rho (get ref) MkRole)
       role <- roleCheck rho (RoleRef ref)
       scope <- check
                  rho
                  delta
                  gamma
                  scope
       pure (DefRole scope)


check rho delta gamma (TypeDef fc ref val scope)
  = do (ty ** tm) <- typeCheck delta val

       scope <- check
                  rho
                  (extend delta (get ref) ty)
                  gamma
                  scope
       pure (DefType tm scope)

check rho delta gamma (FuncDef fc ref f scope)
  = do (FUNC as r ** f) <- funcCheck rho delta gamma f
         | (ty ** _) => throwAt fc (FunctionExpected ty)

       scope <- check
                  rho
                  delta
                  (extend gamma (get ref) (FUNC as r))
                  scope

       tyTm <- typeReflect delta (FUNC as r)
       pure (DefFunc tyTm f scope)


check rho delta gamma (Main fc f)
  = do (FUNC Nil UNIT ** f) <- funcCheck rho delta gamma f
         | (ty ** _) => mismatchAt fc (FUNC Nil UNIT) ty

       pure (Main f)


export
progCheck : (r : Prog) -> Olai Program
progCheck p
  = check Nil Nil Nil (view p)

-- [ EOF ]
