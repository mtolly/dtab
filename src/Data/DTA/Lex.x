{
-- | Generated lexer for text @.dta@ files.
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}
module Data.DTA.Lex (scan, Token(..), AlexPosn(..)) where

import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import Data.Typeable
import Data.Data
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

-- Whitespace and line comments.
$white+ ;
\; [^\n]* ;

-- Numbers. Longest match rule means N.N is float, not int.
\-? $digit+ { \pn str -> (pn, Int $ read str) }
\-? $digit+ (\. $digit+)? ('e' $digit+)? { \pn str -> (pn, Float $ read str) }

-- Variable names.
\$ ($alpha | $digit | _)+ { \pn str -> (pn, Var $ B8.pack $ tail str) }

-- This reserved word needs to come before the general keyword rule.
"kDataUnhandled" { \pn _ -> (pn, Unhandled) }
-- Raw keywords. Note: these can start with digits, like "3sand7s", as long as
-- they also have letters in them.
($alpha | $digit | _ | \/ | \.)+ { \pn str -> (pn, Key $ B8.pack str) }
-- Quoted keywords.
' ([^'] | \\')* ' { \pn str -> (pn, Key $ B8.pack $ getKeyword str) }

-- Quoted strings.
\" ([^\"] | \\\")* \" { \pn str -> (pn, String $ B8.pack $ read str) }

-- Preprocessor commands.
\#ifdef { \pn _ -> (pn, IfDef) }
\#else { \pn _ -> (pn, Else) }
\#endif { \pn _ -> (pn, EndIf) }
\#define { \pn _ -> (pn, Define) }
\#include { \pn _ -> (pn, Include) }
\#merge { \pn _ -> (pn, Merge) }
\#ifndef { \pn _ -> (pn, IfNDef) }

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
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Reads a single-quoted string, by converting it to a double-quoted one.
getKeyword :: String -> String
getKeyword = read . go where
  go ('\'':xs) = '"' : go xs        -- string begin/end -> double-quote
  go ('"':xs) = '\\' : '"' : go xs  -- double-quote gets escaped
  go ('\\':x:xs) = '\\' : x : go xs -- any escaped char can remain escaped
  go (x:xs) = x : go xs             -- all other chars are unchanged
  go [] = []

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

}
