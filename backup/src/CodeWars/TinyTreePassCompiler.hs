module CodeWars.TinyTreePassCompiler where

import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as Map

import Data.List

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



compareActionPriority:: Token -> Token -> Ordering
compareActionPriority x y = compare (getPriorityValue x) (getPriorityValue y)

getPriorityValue :: Token -> Int
getPriorityValue (TChar '+') = 9
getPriorityValue (TChar '-') = 9
getPriorityValue (TChar '*') = 8
getPriorityValue (TChar '/') = 8
getPriorityValue (TChar '(') = 3
getPriorityValue (TChar ')') = 3
getPriorityValue (TChar '[') = 1
getPriorityValue (TChar ']') = 1


pass1 :: String -> AST
pass1 xs = pass1' ts 10 Map.empty [] [] 
  where ts = tokenize xs


pass1' :: [Token] -> Int -> (Map String AST) -> [Token] -> [AST] -> AST
pass1' [] _ _ [] (a:_) = a
pass1' [] _ _ (x:[]) (a:a':_) = getOpAst x a' a
pass1' [] p argMap (t:ts) as =
  let (ts',as') = doOp ts t as
  in pass1' [] p argMap ts' as'
pass1' ((TChar '['):xs) _ argMap ts as = pass1' xs 1 argMap ts as
pass1' (arg@(TStr ss):xs) 1 argMap ts as =
  let argMap' = Map.insert ss (Arg (Map.size argMap)) argMap
    in pass1' xs 1 argMap' ts as
pass1' (arg@(TStr ss):xs) p argMap ts as =
  let argP = (Map.!) argMap ss
    in pass1' xs p argMap ts (argP:as)
pass1' ((TChar ']'):xs) 1 argMap ts as = pass1' xs 10 argMap ts as
pass1' (arg@(TInt imm):xs) p argMap ts as = pass1' xs p argMap ts ((Imm imm):as)
pass1' ((TChar '('):xs) p argMap ts as =
  let (xs',ys') = span (\a -> (TChar ')') /= a) xs
      newAst = pass1' xs' 10 argMap [] []
  in pass1' (tail ys') p argMap ts (newAst:as)
pass1' (op@(TChar x):xs) p argMap ts as =
   let (ts'@(t':_),as') = doOp ts op as
   in pass1' xs (getPriorityValue t') argMap (op:ts) as


doOp :: [Token] -> Token -> [AST] -> ([Token], [AST])
doOp [] t as = (t:[], as)
doOp xss@(x:xs) t (a:a':as) =
  let cpP = compareActionPriority x t
  in if cpP == GT then ((t:xss), as) else doOp xs t ((getOpAst x a a'):as)
--doOp xss@(x:xs) t (a:as) = ((t:xss),(a:as))


getOpAst :: Token -> AST -> AST -> AST
getOpAst (TChar '+') x y = (Add x y)
getOpAst (TChar '-') x y = (Sub x y)
getOpAst (TChar '*') x y = (Mul x y)
getOpAst (TChar '/') x y = (Div x y)


  


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
