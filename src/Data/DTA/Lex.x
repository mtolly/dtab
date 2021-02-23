{
-- | Generated lexer for text @.dta@ files.
{-# OPTIONS_GHC -w #-}
module Data.DTA.Lex (scan, Token(..), AlexPosn(..)) where

import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
}

%wrapper "posn"

$digit = 0-9

tokens :-

-- Whitespace and line comments.
$white+ ;
\; [^\n]* ;

-- Preprocessor commands.
\#ifdef { \pn _ -> (pn, IfDef) }
\#else { \pn _ -> (pn, Else) }
\#endif { \pn _ -> (pn, EndIf) }
\#define { \pn _ -> (pn, Define) }
\#include { \pn _ -> (pn, Include) }
\#merge { \pn _ -> (pn, Merge) }
\#ifndef { \pn _ -> (pn, IfNDef) }

-- Numbers. Longest match rule means N.N is float, not int.
(\+ | \-)? $digit+ { \pn str -> (pn, Int $ read $ dropWhile (== '+') str) }
(\+ | \-)? $digit+ (\. $digit+)? (e \-? $digit+)? { \pn str -> (pn, Float $ read $ dropWhile (== '+') str) }
(\+ | \-)? \. $digit+ (e \-? $digit+)? { \pn str -> (pn, Float $ read $ case dropWhile (== '+') str of
  '-' : rest -> '-' : '0' : rest
  s          -> '0' : s
  )
}

-- Variable names.
\$ (. # $white # [ \( \) \{ \} \[ \] ])+ { \pn str -> (pn, Var $ B8.pack $ tail str) }

-- This reserved word needs to come before the general keyword rule.
"kDataUnhandled" { \pn _ -> (pn, Unhandled) }
-- Quoted strings.
\" ([^\"] | \n)* \" { \pn str -> (pn, String $ B8.pack $ readString str) }
-- Quoted keywords.
' ([^'] | \\')* ' { \pn str -> (pn, Key $ B8.pack $ readQuotedSymbol str) }
-- Raw keywords. Note: these can start with digits, like "3sand7s", as long as
-- they also have letters in them.
(. # $white # [ \( \) \{ \} \[ \] ])+ { \pn str -> (pn, Key $ B8.pack str) }

-- Subtrees.
\( { \pn _ -> (pn, LParen) }
\) { \pn _ -> (pn, RParen) }
\{ { \pn _ -> (pn, LBrace) }
\} { \pn _ -> (pn, RBrace) }
\[ { \pn _ -> (pn, LBracket) }
\] { \pn _ -> (pn, RBracket) }

{

data Token
  = Int Int32
  | Float Float
  | Var B8.ByteString
  | Key B8.ByteString
  | Unhandled
  | IfDef
  | Else
  | EndIf
  | LParen
  | RParen
  | LBrace
  | RBrace
  | String B8.ByteString
  | LBracket
  | RBracket
  | Define
  | Include
  | Merge
  | IfNDef
  deriving (Eq, Ord, Show, Read)

mirrorTail :: [a] -> [a]
mirrorTail = go . drop 1 where
  go []       = [] -- shouldn't happen
  go [_]      = []
  go (x : xs) = x : go xs

{-
Escape sequences differ slightly in different games + other software like Magma.
To support encoding any character, we implement the following:
  \q means double quote inside double quotes, or single quote inside single quotes
  \n means newline in either double or single quotes
  \\ means literal backslash
Backslash not followed by backslash n or q also becomes a literal backslash.
-}

readQuotedSymbol :: String -> String
readQuotedSymbol = go . mirrorTail where
  go ('\\' : 'n'  : rest) = '\n' : go rest
  go ('\\' : 'q'  : rest) = '\'' : go rest
  go ('\\' : '\\' : rest) = '\\' : go rest
  go ""                   = ""
  go (c : rest)           = c : go rest

readString :: String -> String
readString = go . mirrorTail where
  go ('\\' : 'n'  : rest) = '\n' : go rest
  go ('\\' : 'q'  : rest) = '"'  : go rest
  go ('\\' : '\\' : rest) = '\\' : go rest
  go ""                   = ""
  go (c : rest)           = c : go rest

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

}
