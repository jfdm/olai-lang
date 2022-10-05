||| Views on sessions.
|||
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
|||
||| Let's be smart about the shape of the AST for types.
|||
||| We reduce the *raw* AST to a single tree in which the node values
||| represent extra information about the children.
module Ola.Raw.Sessions.View

import Data.List1

import Toolkit.Data.Location

import Ola.Raw.Roles

import Ola.Types
import Ola.Raw.Types

import Ola.Raw.Types.View

import Ola.Raw.Sessions

%default total

mutual
  public export
  data Branches : (bs : List (String, Raw.Ty, Raw.Session))
               -> Type
    where
      Nil : Branches Nil
      Add : (s    : String)
         -> (ty   : Ty t)
         -> (cont : Sessions g)
         -> (rest : Branches bs)
                 -> Branches ((s,t,g)::bs)

  public export
  data Branches1 : (bs : List1 (String, Raw.Ty, Raw.Session))
                      -> Type
    where
      B1 : Branches  (b::bs)
        -> Branches1 (b:::bs)

  public export
  data Sessions : (r : Raw.Session)
                    -> Type
    where
      End : (fc : FileContext)
               -> Sessions (Null fc END)
      Call : (fc : FileContext)
          -> (r  : Ref)
               -> Sessions (Null fc (CALL r))

      Rec : (fc : FileContext)
         -> (r  : Ref)
         -> (scope : Sessions b)
                  -> Sessions (Un fc (REC r) b)

      Choice : (fc : FileContext)
            -> (s  : Raw.Role)
            -> (r  : Raw.Role)
            -> (branches : Branches1 bs)
                        -> Sessions (N1 fc (CHOICE s r) bs)

mutual
  viewBs : (bs : List (String, Raw.Ty, Raw.Session)) -> Branches bs
  viewBs [] = []
  viewBs ((x,y,z) :: xs)
    = Add x (view y) (view z) (viewBs xs)

  viewBs1 : (bs : List1 (String, Raw.Ty, Raw.Session)) -> Branches1 bs
  viewBs1 (head ::: tail)
    = B1 (viewBs (head :: tail))

  export
  view : (r : Raw.Session)
           -> Sessions r
  view (Null fc END)
    = End fc
  view (Null fc (CALL x))
    = Call fc x
  view (Un fc (REC r) z)
    = Rec fc r (view z)
  view (N1 fc (CHOICE x y) xs)
    = Choice fc x y (assert_total $ viewBs1 xs)

-- [ EOF ]
