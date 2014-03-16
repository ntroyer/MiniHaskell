----------------------------------------------------------------
-- Computer Science 320 (Fall, 2012)
-- Concepts of Programming Languages
--
-- Assignment 6
--   Exp.hs

----------------------------------------------------------------
-- Abstract Syntax for the mini-Haskell Interpreter

module Exp (Oper(..), Exp(..), subst, fv, fixExp, opsWithStrings, opsWithStrings2) 
  where

import Env
  
data Oper = Plus | Times 
          | Equal 
          | And | Or | Not 
          | Head | Tail | Cons
  deriving Eq

data Exp = Unit | Nil
         | N Int | B Bool | Var String | Op Oper
         | App Exp Exp
         | LamUnit Exp
         | Lam String Exp
         | If Exp Exp Exp
         | Let [(String, Exp)] Exp

----------------------------------------------------------------
-- Printing functions for Abstract Syntax

ops       = [Not, Head, Tail,
             Plus, Times, Equal, And, Or, Cons]
opStrings = ["not", "head", "tail",
             "(+)", "(*)", "(==)", "(&&)", "(||)", "(:)"]
opsWithStrings = zip ops opStrings

showOper op = fOp op opsWithStrings
  where
    fOp op ((o,n):ons) = if (o == op) then n else (fOp op ons)
    fOp op []           = "impossible"

-- Convenient list for the parser.
ops2       = [Plus, Times, Equal, And, Or, Cons]
opStrings2 = ["+)", "*)", "==)", "&&)", "||)", ":)"]
opsWithStrings2 = zip ops2 opStrings2

showExp wh Unit  = wh ++ "()"
showExp wh Nil   = wh ++ "[]"
showExp wh (N n) = wh ++ (show n)
showExp wh (B b) = wh ++ (show b)
showExp wh (Var x) = wh ++ x
showExp wh (Op o) = wh ++ (show o)
showExp wh (App e1 e2) = "(" ++ (showExp wh e1) ++ 
                         " " ++ (showExp wh e2) ++ ")"
showExp wh (LamUnit e) = "\\() -> " ++ (showExp wh e)
showExp wh (Lam x e) = "\\" ++ x ++ " -> " ++ (showExp wh e)
showExp wh (If e1 e2 e3) = "if " ++ (showExp wh e1) ++ 
                           " then " ++ (showExp wh e2) ++ 
                           " else " ++ (showExp wh e3)
showExp wh (Let xses e) = "let\n" ++ (showBinds wh xses) ++ 
                          "in \n  " ++ (showExp wh e)

showBinds wh [] = "impossible"
showBinds wh [(n, e)] = "  " ++ n ++ " = " ++ (showExp wh e) ++ "\n"
showBinds wh ((n, e):xns) = "  " ++ n ++ " = " ++ (showExp wh e) ++ 
                                 ";\n" ++ (showBinds wh xns)

instance Show Oper where
  show = showOper

instance Show Exp where
  show = showExp []

--------------------------------------

fixExp = Lam "f" 
         (App (Lam "x" (App (Var "f") (Lam "y" 
                  (App (App (Var "x") (Var "x")) (Var "y")))))
              (Lam "x" (App (Var "f") (Lam "y" 
                  (App (App (Var "x") (Var "x")) (Var "y"))))))

fv :: String -> Exp -> Bool
fv x (Var x')      = x == x'
fv x (Lam x' e)    = if (x == x') then False else fv x e
fv x (LamUnit e)   = fv x e
fv x (App e1 e2)   = (fv x e1) || (fv x e2)
fv x (If e1 e2 e3) = (fv x e1) || (fv x e2) || (fv x e3)
fv x (Let [] be)   = fv x be
fv x (Let ((x',e):nes) be) =
  if (x == x') then False else (fv x (Let nes be)) || (fv x e)
fv x _        = False
  
------Problem Set 7, Problem 1, Part 1

noLets :: Exp -> Exp
noLets (Let ((x,e):xs) be) = 
  if fv x e then
    App (Lam x (noLets (Let xs be))) (App fixExp (Lam x e))
  else
    App (Lam x (noLets (Let xs be))) e
noLets (App e1 e2) = App (noLets e1) (noLets e2)
noLets (LamUnit e1) = LamUnit (noLets e1)
noLets (Lam str e1) = Lam str (noLets e1)
noLets (If e1 e2 e3) = If (noLets e1) (noLets e2) (noLets e3)
noLets exp = exp

--Problem Set 7, Problem 1, Part 2

subst :: String -> Exp -> Exp -> Exp

subst s e1 (Var s1)     = if (s == s1) then e1 else Var s1

subst s e1 (If e2 e3 e4) = If (subst s e1 e2) (subst s e1 e3) (subst s e1 e4)
subst s e1 (App e2 e3)   = App (subst s e1 e2) (subst s e1 e3)

subst s e1 (LamUnit e2) = LamUnit (subst s e1 e2)
subst s e1 (Lam s' e2) = Lam s' (
	if s == s'	then e2
	else (subst s e1 e2))
	
subst s e1 (Let ((x,e):xs) e2) =
  if (s == x) then
    Let ((x,e):xs) e2
  else
    let e3 = subst s e1 e
        (Let xs' e2') = subst s e1 (Let xs e2)
    in
       Let ((x,e3):xs') e2'
	   
subst s e1 (Let [] be) = Let [] (subst s e1 be)

subst s e1 e = e