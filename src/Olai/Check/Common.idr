|||
||| Module    : Common.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Check.Common

import public Decidable.Equality
import public Data.Singleton
import public Data.Fin

import public Toolkit.Decidable.Informative
import public Toolkit.Data.Location

import public Toolkit.Data.List.AtIndex
import public Toolkit.DeBruijn.Context
import public Toolkit.DeBruijn.Context.Item
import public Toolkit.DeBruijn.Renaming

import Data.List.Elem

import Olai.Core
import Olai.Types
import Olai.Terms.Types
import Olai.Terms.Exprs

throw : Typing.Error -> Olai a
throw = (throw . TyCheck . E)

export
throwAt : FileContext -> Typing.Error -> Olai a
throwAt l e = throw $ TyCheck (S l (E e))

export
unknown : FileContext -> Olai a
unknown fc = Common.throwAt fc Unknown

export
mismatch : (g,e : Ty.Base) -> Olai a
mismatch g e = Common.throw $ Mismatch g e

export
notBound : Ref -> Olai a
notBound r = throw (NotBound r)

export
mismatchAt : (fc : FileContext) -> (g,e : Ty.Base) -> Olai a
mismatchAt fc g e = throwAt fc (Mismatch g e)

namespace Maybe
  export
  embedAt : FileContext
         -> Typing.Error
         -> Maybe a
         -> Olai   a
  embedAt _ _ (Just prf)
    = pure prf
  embedAt fc err Nothing
    = throwAt fc err

namespace Decidable
  export
  embedAt : FileContext
         -> Typing.Error
         -> Dec     a
         -> Olai a
  embedAt _ _ (Yes prf)
    = pure prf
  embedAt fc err (No _)
    = throwAt fc err

  namespace Informative

    export
    embedAt : FileContext
           -> Typing.Error
           -> DecInfo e a
           -> Olai   a
    embedAt _ _ (Yes prfWhy)
      = pure prfWhy
    embedAt fc err (No _ _)
      = throwAt fc err

    export
    embedAtInfo : FileContext
               -> Typing.Error
               -> DecInfo e a
               -> Olai   a
    embedAtInfo = embedAt

export
compare : (fc  : FileContext)
       -> (a,b : Ty.Base)
              -> Olai (a = b)
compare fc a b
  = embedAt fc (Mismatch a b)
               (decEq    a b)

public export
data The : (rs    : List Ty.Role)
        -> (ds,gs : List Ty.Base)
                 -> Type
  where
    T : (type : Ty.Base)
     -> (ty   : Ty      ds    type )
     -> (e    : Expr rs ds gs type)
             -> The  rs ds gs
-- [ EOF ]
