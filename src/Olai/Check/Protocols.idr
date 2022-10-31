|||
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Check.Protocols

import Toolkit.Data.Location
import Toolkit.Data.DList
import Toolkit.Data.List.AtIndex

import Data.Singleton

import Olai.Types
import Olai.Core

import Olai.Raw.Roles

import Olai.Raw.Protocols
import Olai.Raw.Protocols.View

import Olai.Raw.Types
import Olai.Raw.Types.View

import Olai.Check.Common
import Olai.Check.Types
import Olai.Check.Roles

import Olai.Terms.Vars
import Olai.Terms.Roles
import Olai.Terms.Types
import Olai.Terms.Protocols

%default total

mutual
  checkBS : {ts : List Base}
         -> {rs : List Ty.Role}
         -> {ks : List Kind}
         -> (kinds : Context Kind    ks)
         -> (types : Context Ty.Base ts)
         -> (roles : Context Ty.Role rs)
         -> (bs    : Branches bs')
                  -> Olai (DPair (Global.Branches ks    rs)
                                (Branches        ks ts rs))
  checkBS kinds types roles []
    = pure (Nil ** Nil)

  checkBS kinds types roles (Add s ty cont rest)
    = do (ty ** tm) <- typeCheck types ty
         (g  ** tmg) <- check kinds types roles cont
         (bs ** tmb) <- checkBS kinds types roles rest
         pure (_ ** B s tm tmg :: tmb)

  checkB1 : {ts : List Base}
         -> {rs : List Ty.Role}
         -> {ks : List Kind}
         -> (kinds : Context Kind    ks)
         -> (types : Context Ty.Base ts)
         -> (roles : Context Ty.Role rs)
         -> (bs1   : Branches1 bs)
                  -> Olai (DPair (Global.Branches1 ks    rs)
                                (Branches1        ks ts rs))
  checkB1 kinds types roles (B1 bs)
    = do ((b::bs) ** tm) <- checkBS kinds types roles bs
           | (Nil ** _) => throw (Generic "internal checking nil branch when branches expected")
         pure (b:::bs ** Bs1 tm)

  check : {ts : List Base}
       -> {ks : List Kind}
       -> {rs : List Ty.Role}
       -> (kinds : Context Kind    ks)
       -> (types : Context Ty.Base ts)
       -> (roles : Context Ty.Role rs)
       -> (syn   : Protocols g)
               -> Olai (DPair (Ty.Global ks    rs)
                             (Global    ks ts rs))
  check kinds types roles (End fc)
    = pure (_ ** End)

  check kinds types roles (Call fc r)
    = do prf <- embedAtInfo
                    (span r)
                    (NotBound r)
                    (Lookup.lookup (get r) kinds)
         let (R ** (loc ** prfN)) = deBruijn prf
         pure (_ ** Call (V loc prfN))

  check kinds types roles (Rec fc r scope)
    = do (g ** scope) <- check (extend kinds (get r) R) types roles scope
         pure (_ ** Rec scope)

  check kinds types roles (Choice fc s@(RoleRef s') r@(RoleRef r') branches)
    = do (MkRole ** stm) <- roleCheck roles s
         (MkRole ** rtm) <- roleCheck roles r
         (bs ** tm) <- checkB1 kinds types roles branches
         case Index.decEq stm rtm of
           Yes (Same Refl Refl) => throwAt fc (MismatchRole s' r')
           No prf => pure (_ ** Choice stm rtm prf tm)

export
protocolCheck : {ts    : List Base}
            -> {rs    : List Ty.Role}
            -> (types : Context Ty.Base ts)
            -> (roles : Context Ty.Role rs)
            -> (sesh  : Protocols s)
                     -> Olai (DPair (Ty.Global Nil    rs)
                                      (Global Nil ts rs))
protocolCheck
  = check Nil


namespace Raw
  export
  protocolCheck : {ts    : List Ty.Base}
              -> {rs    : List Ty.Role}
              -> (types : Context Ty.Base ts)
              -> (roles : Context Ty.Role rs)
              -> (sesh  : Raw.Protocol)
                       -> Olai (DPair (Ty.Global Nil    rs)
                                        (Global Nil ts rs))
  protocolCheck types roles s
    = check Nil types roles (view s)


-- [ EOF ]
