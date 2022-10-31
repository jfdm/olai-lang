||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Parser.Protocols

import Data.List1
import Data.Maybe
import Data.Either

import Olai.Core
import Olai.Types

import Olai.Lexer
import Olai.Parser.API

import Olai.Raw.Roles
import Olai.Raw.Types
import Olai.Raw.Protocols

import Olai.Parser.Roles
import Olai.Parser.Types

%default total


end : Rule Raw.Protocol
end
  = do s <- Toolkit.location
       keyword "end"
       e <- Toolkit.location
       pure (Null (newFC s e) END)

call : Rule Raw.Protocol
call
  = do s <- Toolkit.location
       keyword "call"
       symbol "("
       r <- Olai.ref
       symbol ")"
       e <- Toolkit.location
       pure (Null (newFC s e) (CALL r))

mutual
  rec : Rule Raw.Protocol
  rec
    = do s <- Toolkit.location
         keyword "rec"
         symbol "("
         r <- Olai.ref
         symbol ")"
         symbol "."
         sesh <- protocol
         e <- Toolkit.location
         pure (Un (newFC s e) (REC r) sesh)

  branch : Rule (String, Raw.Ty, Raw.Protocol)
  branch
    = do l <- Olai.ref
         symbol "("
         t <- type
         symbol ")"
         symbol "."
         s <- protocol
         pure (get l,t,s)

  choice : Rule Raw.Protocol
  choice
    = do s <- Toolkit.location
         a <- role
         symbol "==>"
         b <- role
         symbol "{"
         bs <- sepBy1 (symbol "|") branch
         symbol "}"
         e <- Toolkit.location
         pure (N1 (newFC s e) (CHOICE a b) bs)

  export
  protocol : Rule Raw.Protocol
  protocol
     =  end
    <|> call
    <|> choice
    <|> rec

-- [ EOF ]
