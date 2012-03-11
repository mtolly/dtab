module Data.DTA.Parse (pDTA, readDTA, hReadDTA) where

import Data.DTA
import qualified Data.DTA.Lex as L
import System.IO

-- | Read a DTA (text) from a file.
readDTA :: FilePath -> IO DTA
readDTA = fmap (pDTA . L.scan) . readFile

-- | Read a DTA (text) from a handle.
hReadDTA :: Handle -> IO DTA
hReadDTA = fmap (pDTA . L.scan) . hGetContents

showPn :: L.AlexPosn -> String
showPn (L.AlexPn _ l c) = show l ++ " line, " ++ show c ++ " column"

errorAt :: L.AlexPosn -> String -> a
errorAt pn str = error $ "Parse error at " ++ showPn pn ++ ": " ++ str

errorEOF :: String -> a
errorEOF str = error $ "Parse error at EOF: " ++ str

{- |
Parse a sequence of lexer tokens as a DTA structure.
-}
pDTA :: [L.PToken] -> DTA
pDTA pts = case pTree pts of
  (tr, []) -> DTA 0 tr
  (_, L.PToken pn t : _) ->
    errorAt pn $ "unexpected " ++ show t ++ ", expecting EOF"

pTree :: [L.PToken] -> (Tree, [L.PToken])
pTree pts = case pChunks pts of
  (chks, rest) -> (Tree 0 chks, rest)

pChunks :: [L.PToken] -> ([Chunk], [L.PToken])
pChunks pts = case pChunk pts of
  Nothing -> ([], pts)
  Just (chk, pts') -> case pChunks pts' of
    (chks, pts'') -> (chk:chks, pts'')

pChunk :: [L.PToken] -> Maybe (Chunk, [L.PToken])
pChunk [] = Nothing
pChunk (L.PToken pn tok : pts) = case tok of
  L.Int i -> Just (Int i, pts)
  L.Float f -> Just (Float f, pts)
  L.Var t -> Just (Var t, pts)
  L.Key t -> Just (Key t, pts)
  L.Unhandled -> Just (Unhandled, pts)
  L.IfDef -> case pts of
    [] -> errorEOF "expecting keyword after #ifdef"
    (L.PToken _ (L.Key t) : pts') -> Just (IfDef t, pts')
    (L.PToken pn' _ : _) -> errorAt pn' "expecting keyword after #ifdef"
  L.Else -> Just (Else, pts)
  L.EndIf -> Just (EndIf, pts)
  
  L.LParen -> case pTree pts of
    (tr, L.PToken _ L.RParen : pts') -> Just (Parens tr, pts')
    (_, []) -> errorEOF $ "expecting ')' to match '(' at " ++ showPn pn
    (_, L.PToken pn' _ : _) ->
      errorAt pn' $ "expecting ')' to match '(' at " ++ showPn pn
  L.RParen -> Nothing
  
  L.LBrace -> case pTree pts of
    (tr, L.PToken _ L.RBrace : pts') -> Just (Braces tr, pts')
    (_, []) -> errorEOF $ "expecting '}' to match '{' at " ++ showPn pn
    (_, L.PToken pn' _ : _) ->
      errorAt pn' $ "expecting '}' to match '{' at " ++ showPn pn
  L.RBrace -> Nothing
  
  L.String t -> Just (String t, pts)
  
  L.LBracket -> case pTree pts of
    (tr, L.PToken _ L.RBracket : pts') -> Just (Brackets tr, pts')
    (_, []) -> errorEOF $ "expecting ']' to match '[' at " ++ showPn pn
    (_, L.PToken pn' _ : _) ->
      errorAt pn' $ "expecting ']' to match '[' at " ++ showPn pn
  L.RBracket -> Nothing
  
  L.Define -> case pts of
    [] -> errorEOF "expecting keyword after #define"
    (L.PToken _ (L.Key t) : pts') -> Just (Define t, pts')
    (L.PToken pn' _ : _) -> errorAt pn' "expecting keyword after #define"
  L.Include -> case pts of
    [] -> errorEOF "expecting keyword after #include"
    (L.PToken _ (L.Key t) : pts') -> Just (Include t, pts')
    (L.PToken pn' _ : _) -> errorAt pn' "expecting keyword after #include"
  L.Merge -> case pts of
    [] -> errorEOF "expecting keyword after #merge"
    (L.PToken _ (L.Key t) : pts') -> Just (Merge t, pts')
    (L.PToken pn' _ : _) -> errorAt pn' "expecting keyword after #merge"
  L.IfNDef -> case pts of
    [] -> errorEOF "expecting keyword after #ifndef"
    (L.PToken _ (L.Key t) : pts') -> Just (IfNDef t, pts')
    (L.PToken pn' _ : _) -> errorAt pn' "expecting keyword after #ifndef"
