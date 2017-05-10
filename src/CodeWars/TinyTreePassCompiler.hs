module CodeWars.TinyTreePassCompiler where

import Data.Set(Set)
import qualified Data.Set as Set

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1


pass1 :: String -> AST
pass1 xs = pass1' ts False [] [] 
  where ts = tokenize xs


pass1' :: [Token] -> Bool -> (Set (TStr String) AST) -> [Token] -> [AST] -> AST
pass1' [] _ _ _ x = x
pass1' ((TChar '['):xs) _ _ _ x = pass1' xs True _ _ _
pass1' arg@(TStr _):xs True argMap _ x =
  let argMap' = Set.insert arg (Arg (Set.size argMap))
    in pass1' xs True argMap' _ _
pass1' (TChar ']'):xs True _ _ x = pass1' xs False _ _ _


mergeAST :: [Token] -> AST
mergeAST = undefined

pass2 :: AST -> AST
pass2 xm@(Imm x) = xm
pass2 xm@(Arg x) = xm
pass2 (Add x y) =
  let x' = pass2 x
      y' = pass2 y
  in if isImm x' && isImm y' then Imm ((getImm x') + (getImm y')) else Add x' y'
pass2 (Sub x y) =
  let x' = pass2 x
      y' = pass2 y
  in if isImm x' && isImm y' then Imm ((getImm x') - (getImm y')) else Sub x' y'
pass2 (Mul x y) =
  let x' = pass2 x
      y' = pass2 y
  in if isImm x' && isImm y' then Imm ((getImm x') * (getImm y')) else Mul x' y'
pass2 (Div x y) = 
  let x' = pass2 x
      y' = pass2 y
  in if isImm x' && isImm y' then Imm (div (getImm x') (getImm y')) else Div x' y'

pass3 :: AST -> [String]
pass3 = undefined


isImm :: AST -> Bool
isImm (Imm _) = True
isImm _ = False


getImm :: AST -> Int
getImm (Imm x) = x
