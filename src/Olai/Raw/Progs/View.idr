||| Turn the abstract AST to something more precise.
|||
||| Module    : Olai.Raw.Progs.View
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Raw.Progs.View

import Toolkit.Data.Location
import Toolkit.Data.DList

import Olai.Types

import Olai.Raw.Roles

import Olai.Raw.Types
import Olai.Raw.Types.View

import Olai.Raw.Protocols
import Olai.Raw.Protocols.View

import Olai.Raw.Exprs
import Olai.Raw.Exprs.View

import Olai.Raw.Stmts
import Olai.Raw.Stmts.View

import Olai.Raw.Funcs
import Olai.Raw.Funcs.View

import Olai.Raw.Progs

%default total

public export
data Prog : (s : Raw.Prog) -> Type where
  RoleDef : (fc    : FileContext)
         -> (ref   : Ref)
         -> (scope : Prog body)
                  -> Prog (Un fc (DEFROLE ref) body)

  TypeDef : (fc    : FileContext)
         -> (ref   : Ref)
         -> (val   : Ty t)
         -> (scope : Prog body)
                  -> Prog (Un fc (DEFTYPE ref t) body)

  SeshDef : (fc    : FileContext)
         -> (ref   : Ref)
         -> (sesh  : Protocols s)
         -> (scope : Prog body)
                  -> Prog (Un fc (DEFSESH ref s) body)

  FuncDef : (fc    : FileContext)
         -> (ref   : Ref)
         -> (val   : Func f)
         -> (scope : Prog body)
                  -> Prog (Un fc (DEFFUNC ref f) body)
  Main : (fc : FileContext)
      -> (body : Func f)
              -> Prog (Null fc (MAIN f))

export
view : (s : Raw.Prog) -> Prog s
view (Null fc (MAIN f))
  = Main fc (view f)

view (Un fc (DEFTYPE x y) rem)
  = TypeDef fc x (view y) (view rem)

view (Un fc (DEFFUNC x y) rem)
  = FuncDef fc x (view y) (view rem)

view (Un fc (DEFROLE x) rem)
  = RoleDef fc x (view rem)

view (Un fc (DEFSESH ref s) rem)
  = SeshDef fc ref (view s) (view rem)

export
getFC : {s : Prog} -> Prog s -> FileContext
getFC {s} _ = getFC s

-- [ EOF ]
