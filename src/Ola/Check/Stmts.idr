||| Type-checker for statements.
|||
||| Module    : Types.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Ola.Check.Stmts

import Toolkit.Data.Location

import Ola.Types
import Ola.Core

import Ola.Raw.Types
import Ola.Raw.Exprs

import Ola.Raw.Stmts
import Ola.Raw.Stmts.View

import Ola.Check.Common

import Ola.Check.Types
import Ola.Check.Exprs

import Ola.Terms.Vars
import Ola.Terms.Types
import Ola.Terms.Exprs
import Ola.Terms.Stmts

%default total

public export
data Result : (ds,gs : List Types.Ty)
           -> (ty    : Types.Ty)
                    -> Type
  where
    R : {new  : List Types.Ty}
     -> (newG : Context Types.Ty new)
     -> (type : Types.Ty)
     -> (term : Stmt   ds gs new type)
     -> (prf  : type = ty')
             -> Result ds gs ty'

-- stack in stack out

check : {s     : Stmt}
     -> {ds,gs : List Types.Ty}
     -> (delta : Context Types.Ty ds)
     -> (gamma : Context Types.Ty gs)
     -> (ty    : Types.Ty)
     -> (stmt  : Stmt s)
              -> Ola (Result ds gs ty)

-- [ NOTE ] Type inference...
check delta gamma ty (End fc)
  = pure (R gamma ty End Refl)

check delta gamma ty (Ret fc e)
  = do (ty' ** term) <- exprCheck delta gamma e

       Refl <- compare fc ty ty'

       pure (R gamma ty (Return term) Refl)

check delta gamma ty (Print fc e scope)
  = do (STR ** term) <- exprCheck delta gamma e
         | (ty ** _) => mismatchAt fc STR ty

       R g1 ty' scope Refl <- check delta gamma ty scope

       Refl <- compare fc ty ty'

       pure (R g1 ty' (Print term scope) Refl)

check delta gamma ty (LetTy fc this ty' val scope)
  = do (T ty' t val) <- ascript fc delta gamma ty' val

       (R newG tyR tm Refl) <- check
                                 delta
                                 (extend gamma (get this) ty')
                                 ty
                                 scope

       Refl <- compare fc ty tyR

       pure (R newG tyR (Let t val tm) Refl)

check delta gamma ty (Let fc this val' scope)
  = do (T ty' t val) <- ascriptReflect delta gamma val'

       (R newG tyR tm Refl) <- check
                                delta
                                (extend gamma (get this) ty')
                                ty
                                scope

       Refl <- compare fc ty tyR

       pure (R newG tyR (Let t val tm) Refl)


check delta gamma ty (VarTy fc this ty' val scope)

  = do (T ty' t val) <- ascript fc delta gamma ty' val

       (R newG tyR tm Refl) <- check
                                 delta
                                 (extend gamma (get this) (REF ty'))
                                 ty
                                 scope

       Refl <- compare fc ty tyR

       pure (R newG tyR (Let (TyRef t) (Alloc val) tm) Refl)

check delta gamma ty (Var fc this val' scope)

  = do (T ty' t val) <- ascriptReflect delta gamma val'

       (R newG tyR tm Refl) <- check
                                 delta
                                 (extend gamma (get this) (REF ty'))
                                 ty
                                 scope

       Refl <- compare fc ty tyR

       pure (R newG tyR (Let (TyRef t) (Alloc val) tm) Refl)

check delta gamma ty (Mutate fc this value scope)
  = do (REF t ** this) <- exprCheck
                            delta
                            gamma
                            this
         | (ty ** _) => throwAt fc (RefExpected ty)

       (t' ** value) <- exprCheck
                            delta
                            gamma
                            value

       Refl <- compare fc t t'

       (R newG tyR scope Refl) <- check
                                    delta
                                    gamma
                                    ty
                                    scope

       Refl <- compare fc ty tyR

       pure (R newG tyR (Mutate this value scope) Refl)


check delta gamma ty (Run fc val scope)
  = do (t ** value) <- exprCheck
                         delta
                         gamma
                         val
       R newG tyR scope Refl <- check
                                  delta
                                  gamma
                                  ty
                                  scope
       Refl <- compare fc ty tyR

       pure (R newG tyR (Run value scope) Refl)

check delta gamma ty (While fc cond body rest)
  = do (BOOL ** cond) <- exprCheck
                           delta
                           gamma
                           cond
         | (ty ** cond) => mismatchAt fc BOOL ty

       (R newG tyB body Refl) <- check
                                   delta
                                   gamma
                                   ty
                                   body
       Refl <- compare fc ty tyB

       R newG tyR scope Refl <- check
                                  delta
                                  gamma
                                  ty
                                  rest
       Refl <- compare fc tyR tyB
       Refl <- compare fc ty tyR
       pure (R newG tyR (While cond body scope) Refl)


check delta gamma ty (Split fc pair a b scope)
  = do (PAIR aty bty ** pair) <- exprCheck
                                   delta
                                   gamma
                                   pair
         | (ty ** _) => throwAt fc (PairExpected ty)

       R newG tyR scope Refl <- check
                                 delta
                                 (extend (extend gamma (get a) aty)
                                         (get b)
                                         bty)
                                 ty
                                 scope

       Refl <- compare fc ty tyR
       pure (R newG tyR (Split pair scope) Refl)

check delta gamma ty (Match fc cond l scopeL r scopeR rest)
  = do (UNION aty bty ** cond) <- exprCheck
                                     delta
                                     gamma
                                     cond
         | (ty ** _) => throwAt fc (UnionExpected ty)

       R newG tyRA scopeL Refl <- check
                                    delta
                                    (extend gamma (get l) aty)
                                    ty
                                    scopeL
       Refl <- compare fc ty tyRA
       R newG tyBA scopeR Refl <- check
                                    delta
                                    (extend gamma (get r) bty)
                                    ty
                                    scopeR
       Refl <- compare fc tyRA tyBA
       Refl <- compare fc ty tyBA

       R newG tyR rest Refl <- check
                                 delta
                                 gamma
                                 ty
                                 rest
       Refl <- compare fc ty tyR
       pure (R newG tyR (Match cond scopeL scopeR rest) Refl)


check delta gamma ty (Cond fc cond scopeT scopeF rest)

  = do (BOOL ** cond) <- exprCheck
                           delta
                           gamma
                           cond
         | (ty ** _) => mismatchAt fc BOOL ty

       R newG tyRA scopeT Refl <- check
                                    delta
                                    gamma
                                    ty
                                    scopeT
       Refl <- compare fc ty tyRA

       R newG tyBA scopeF Refl <- check
                                    delta
                                    gamma
                                    ty
                                    scopeF

       Refl <- compare fc tyRA tyBA
       Refl <- compare fc ty tyBA

       R newG tyR rest Refl <- check
                                 delta
                                 gamma
                                 ty
                                 rest
       Refl <- compare fc ty tyR
       pure (R newG tyR (Cond cond scopeT scopeF rest) Refl)

export
stmtCheck : {r     : Stmt}
         -> {ds,gs : List Types.Ty}
         -> (delta : Context Types.Ty ds)
         -> (gamma : Context Types.Ty gs)
         -> (ty    : Types.Ty)
         -> (syn   : Stmt r)
                  -> Ola (Result ds gs ty)

stmtCheck
  = check

namespace Raw
  export
  stmtCheck : {ds,gs : List Types.Ty}
           -> (delta : Context Types.Ty ds)
           -> (gamma : Context Types.Ty gs)
           -> (ty    : Types.Ty)
           -> (syn   : Stmt)
                   -> Ola (Result ds gs ty)
  stmtCheck d g ty e
    = check d g ty (view e)

-- [ EOF ]