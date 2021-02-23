{
-- | Generated parser for text @.dta@ files.
module Data.DTA.Parse (parse) where

import Data.DTA.Base
import qualified Data.DTA.Lex as L
}

%name parse
%tokentype { (L.AlexPosn, L.Token) }
%error { parseError }

%token
  int { (_, L.Int $$) }
  float { (_, L.Float $$) }
  var { (_, L.Var $$) }
  sym { (_, L.Sym $$) }
  unhandled { (_, L.Unhandled) }
  ifdef { (_, L.IfDef) }
  else { (_, L.Else) }
  endif { (_, L.EndIf) }
  '(' { (_, L.LParen) }
  ')' { (_, L.RParen) }
  '{' { (_, L.LBrace) }
  '}' { (_, L.RBrace) }
  string { (_, L.String $$) }
  '[' { (_, L.LBracket) }
  ']' { (_, L.RBracket) }
  define { (_, L.Define) }
  include { (_, L.Include) }
  merge { (_, L.Merge) }
  ifndef { (_, L.IfNDef) }

%%

File : Tree { DTA 1 $1 }

Tree : Chunks { Tree 0 $1 }

Chunks : Chunk Chunks { $1 : $2 }
       | { [] }

Chunk : int { Int $1 }
      | float { Float $1 }
      | var { Var $1 }
      | sym { Sym $1 }
      | unhandled { Unhandled }
      | ifdef sym { IfDef $2 }
      | else { Else }
      | endif { EndIf }
      | '(' Tree ')' { Parens $2 }
      | '{' Tree '}' { Braces $2 }
      | string { String $1 }
      | '[' Tree ']' { Brackets $2 }
      | define sym { Define $2 }
      | include sym { Include $2 }
      | merge sym { Merge $2 }
      | ifndef sym { IfNDef $2 }

{

-- | If instead of this error, "Internal Happy error" is sometimes printed, make
-- sure you are using Happy 1.18.7 or later.
parseError :: [(L.AlexPosn, L.Token)] -> a
parseError [] = error "Parse error at EOF"
parseError ((L.AlexPn _ ln col, tok) : _) = error $
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

}
