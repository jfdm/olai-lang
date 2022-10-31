|||
|||
||| Module    : Roles.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Parser.Roles

import Data.List1
import Data.Maybe
import Data.Either

import Olai.Core
import Olai.Types

import Olai.Lexer
import Olai.Parser.API

import Olai.Raw.Roles

%default total

roleVar : Rule Raw.Role
roleVar
  = do r <- Olai.ref
       pure (RoleRef r)

||| Roles
export
role : Rule Raw.Role
role
    = roleVar

-- [ EOF ]
