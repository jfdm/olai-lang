|||
|||
||| Module    : Roles.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Check.Roles

import Toolkit.Data.Location
import Toolkit.Data.DList

import Data.Singleton

import Olai.Types
import Olai.Core

import Olai.Raw.Roles
import Olai.Check.Common

import Olai.Terms.Vars
import Olai.Terms.Roles

%default total


-- ## Check

export
roleCheck : {roles : List Ty.Role}
         -> (ctxt  : Context Ty.Role roles)
         -> (syn   : Raw.Role)
                  -> Olai (DPair Ty.Role (Role roles))

roleCheck ctxt (RoleRef x)
  = do prf <- embedAtInfo
                  (span x)
                  (NotBound x)
                  (Lookup.lookup (get x) ctxt)
       let (r ** (loc ** prfN)) = deBruijn prf
       pure (r ** (V loc prfN))


-- [ EOF ]
