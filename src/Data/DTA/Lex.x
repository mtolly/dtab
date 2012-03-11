{
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}
module Data.DTA.Lex (scan, PToken(..), Token(..), AlexPosn(..)) where

import qualified Data.Text as T
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
\-? $digit+ { \pn str -> PToken pn $ Int $ read str }
\-? $digit+ \. $digit+ ('e' $digit+)? { \pn str -> PToken pn $ Float $ read str }
\-? $digit+ e $digit+ { \pn str -> PToken pn $ Float $ read str }

-- Variable names.
\$ ($alpha | $digit | _)+ { \pn str -> PToken pn $ Var $ T.pack $ tail str }

-- This reserved word needs to come before the general keyword rule.
"kDataUnhandled" { \pn _ -> PToken pn Unhandled }
-- Note: raw keywords can start with digits, like "3sand7s", as long as they
-- also have letters in them.
($alpha | $digit | _ | \/ | \.)+ { \pn str -> PToken pn $ Key $ T.pack str }
-- Quoted keywords.
' ([^'] | \\')* ' { \pn str -> PToken pn $ Key $ T.pack $ getKeyword str }

-- Quoted strings.
\" ([^\"] | \\\")* \" { \pn str -> PToken pn $ String $ T.pack $ read str }

-- Preprocessor commands.
\#ifdef { \pn _ -> PToken pn IfDef }
\#else { \pn _ -> PToken pn Else }
\#endif { \pn _ -> PToken pn EndIf }
\#define { \pn _ -> PToken pn Define }
\#include { \pn _ -> PToken pn Include }
\#merge { \pn _ -> PToken pn Merge }
\#ifndef { \pn _ -> PToken pn IfNDef }

-- Subtrees.
\( { \pn _ -> PToken pn LParen }
\) { \pn _ -> PToken pn RParen }
\{ { \pn _ -> PToken pn LBrace }
\} { \pn _ -> PToken pn RBrace }
\[ { \pn _ -> PToken pn LBracket }
\] { \pn _ -> PToken pn RBracket }

{

data PToken = PToken
  { posn :: AlexPosn
  , token :: Token }

data Token
  = Int Int32
  | Float Float
  | Var T.Text
  | Key T.Text
  | Unhandled
  | IfDef
  | Else
  | EndIf
  | LParen
  | RParen
  | LBrace
  | RBrace
  | String T.Text
  | LBracket
  | RBracket
  | Define
  | Include
  | Merge
  | IfNDef
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Reads a single-quoted string, by converting it to a double-quoted one.
getKeyword :: String -> String
getKeyword = read . go where
  go ('\'':xs) = '"' : go xs        -- string begin/end -> double-quote
  go ('"':xs) = '\\' : '"' : go xs  -- double-quote gets escaped
  go ('\\':x:xs) = '\\' : x : go xs -- any escaped char can remain escaped
  go (x:xs) = x : go xs             -- all other chars are unchanged
  go [] = []

scan :: String -> [PToken]
scan = alexScanTokens

}
