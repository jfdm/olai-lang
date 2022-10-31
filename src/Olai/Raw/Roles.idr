||| AST for Roles.
|||
||| Module    : Roles.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
|||
module Olai.Raw.Roles

import Toolkit.Data.Location

import Olai.Types

%default total

namespace Raw
  public export
  data Role = RoleRef Ref

export
setSource : String -> Raw.Role -> Raw.Role
setSource str (RoleRef x)
  = RoleRef (setSource str x)

export
Show Raw.Role where
  show (RoleRef x)
    = "(RoleRef \{show x}})"

export
getFC : Raw.Role -> FileContext
getFC (RoleRef x) = span x

-- [ EOF ]
