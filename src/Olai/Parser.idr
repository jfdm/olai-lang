|||
|||
||| Module    : Parser.idr
||| Copyright : (c) Jan de Muijnck-Hughes
||| License   : see LICENSE
|||
module Olai.Parser

import Data.List1

import public Olai.Lexer
import Olai.Parser.API

import Olai.Core
import Olai.Types

import Olai.Raw.Roles
import Olai.Raw.Types
import Olai.Raw.Protocols
import Olai.Raw.Exprs
import Olai.Raw.Stmts
import Olai.Raw.Funcs
import Olai.Raw.Progs

import Olai.Parser.Roles
import Olai.Parser.Types
import Olai.Parser.Protocols
import Olai.Parser.Exprs
import Olai.Parser.Stmts
import Olai.Parser.Funcs

%default partial

data Decl = DeclT    FileContext Ref Raw.Ty
          | DeclF    FileContext Ref Raw.Func
          | DeclR    FileContext Ref
          | DeclS    FileContext Ref Raw.Protocol

decls : RuleEmpty (List Decl)
decls
    = many (declTy <|> declFunc <|> declRole <|> declSesh)
  where
    declSesh : Rule Decl
    declSesh
      = do s <- Toolkit.location
           keyword "protocol"
           r <- ref
           symbol "="
           r' <- protocol
           e <- Toolkit.location
           pure (DeclS (newFC s e) r r')

    declRole : Rule Decl
    declRole
      = do s <- Toolkit.location
           keyword "role"
           r <- ref
           e <- Toolkit.location
           pure (DeclR (newFC s e) r)

    declTy : Rule Decl
    declTy
      = do s <- Toolkit.location
           keyword "type"
           r <- ref
           symbol "="
           ty <- type
           e <- Toolkit.location
           pure (DeclT (newFC s e) r ty)

    declFunc : Rule Decl
    declFunc
      = do s <- Toolkit.location
           fs <- func
           e <- Toolkit.location
           pure (DeclF (newFC s e) (fst fs) (snd fs))

main : Rule Prog
main
  = do s <- Toolkit.location
       keyword "main"
       symbol "("
       symbol ")"
       symbol "{"
       b <- body
       symbol "}"
       e <- Toolkit.location
       pure (Null (newFC s e)
                  (MAIN (Null (newFC s e)
                              $ FUNC Nil
                                     (TyNull emptyFC UNIT)
                                     (fst b)
                                     (snd b))))

export
program : Rule Prog
program
    = do ds <- decls
         m <- main
         eoi
         pure (foldr fold m ds)
  where
    fold : Decl -> Raw.Prog -> Raw.Prog
    fold (DeclS fc r s)
      = Un fc (DEFSESH r s)

    fold (DeclR fc r)
      = Un fc (DEFROLE r)

    fold (DeclT fc r ty)
      = Un fc (DEFTYPE r ty)

    fold (DeclF fc r f)
      = Un fc (DEFFUNC r f)

namespace Olai

  export
  fromFile : (fname : String)
                   -> Olai Prog
  fromFile fname
    = do ast <- parseFile (\p => Parse (PError fname p))
                Olai.Lexer
                program
                fname
         pure (setSource fname ast)
           -- @TODO write functers & show for kinds

-- [ EOF ]
