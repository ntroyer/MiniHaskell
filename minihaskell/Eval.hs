----------------------------------------------------------------
-- Computer Science 320 (Fall, 2012)
-- Concepts of Programming Languages
--
-- Assignment 7
--   Eval.hs
--   Nat Troyer

----------------------------------------------------------------
-- Evaluation Functions for the mini-Haskell Interpreter

module Eval (evalExp, ev0, ev) where

import Env
import Err (Error(..))
import Exp (Oper(..), Exp(..), subst, fv, fixExp)
import Val (Val(..))

----------------------------------------------------------------
-- This function is exported to the Main module.

evalExp :: Exp -> Error Val

--Pset 6 Problem 1 code
--evalExp e = ev0

--Pset 6 Problem 3c code
evalExp e = ev e emptyEnv

----------------------------------------------------------------
-- Functions for evaluating operations applied to values.

appOp :: Oper -> Val -> Error Val
appOp Not  (VB b)         = S $ VB $ not b
appOp Head (VListInt (v:vs)) = S $ VN v
appOp Tail (VListInt (v:vs)) = S $ VListInt vs
appOp Head (VListBool (v:vs)) = S $ VB v
appOp Tail (VListBool (v:vs)) = S $ VListBool vs
appOp Head _              = Error "head applied to empty list"
appOp Tail _              = Error "tail applied to empty list"
appOp Not  _              = Error "not applied to non-boolean"
appOp op v2               = S $ Partial op v2

appBinOp :: Oper -> Val -> Val -> Error Val
appBinOp Plus  (VN n) (VN n') = S $ VN (n + n')
appBinOp Times (VN n) (VN n') = S $ VN (n * n')
appBinOp Equal (VN n) (VN n') = S $ VB (n == n')
appBinOp Equal (VB b) (VB b') = S $ VB (b == b')
appBinOp And   (VB b) (VB b') = S $ VB (b && b')
appBinOp Or    (VB b) (VB b') = S $ VB (b || b')
appBinOp Cons  (VN v) (VListInt vs) = S $ VListInt (v:vs)
appBinOp Cons  (VB v) (VListBool vs) = S $ VListBool (v:vs)
appBinOp Cons  (VN v) VNil    = S $ VListInt (v:[])
appBinOp Cons  (VB v) VNil    = S $ VListBool (v:[])
appBinOp op v v' =
  Error $ "binary operator " ++ show op 
           ++ "not defined on arguments " 
           ++ (show v) ++ " and " ++ (show v')

----------------------------------------------------------------
-- Function for applying one value to another.

appVals :: Val -> Val -> Error Val
appVals (VOp op)           v2     = appOp op v2
appVals (Partial op v1 )   v2     = appBinOp op v1 v2

--Pset 6 Problem 3a

appVals (VLam x e env) v2 = ev e (updEnv x v2 env)
appVals (VLamUnit e b) v2 = ev e b

--Pset 7 Problem 2a

appValExp :: Val -> Exp -> Error Val
appValExp (VOp op) e2 =
  case ev0 e2 of
    S v2      -> appVals (VOp op) v2
    Error msg -> Error msg

appValExp (Partial Or  (VB True )) e2 = S (VB True)
appValExp (Partial And (VB False)) e2 = S (VB False)
appValExp (Partial op  v1)         e2 =
  case ev0 e2 of
    S v2      -> appVals (Partial op v1) v2
    Error msg -> Error msg

appValExp (VLamUnit e env) e2 = ev0 e
appValExp (VLam x e env) e2 = ev0 (subst x e2 e)
appValExp v1 e2 = Error $ (show v1) ++ " cannot be applied to an argument"

--Problem Set 7 Problem 3a
--We need this code so we can call ev within appValExp

appValExpEv :: Val -> Exp -> Env Val -> Error Val
appValExpEv (VOp op) e2 env =
  case ev e2 env of 
	S v2      -> appVals (VOp op) v2  
	Error msg -> Error msg

appValExpEv (Partial Or  (VB True )) e2 env = S (VB True)
appValExpEv (Partial And (VB False)) e2 env = S (VB False)
appValExpEv (Partial op  v1)         e2 env =
  case ev e2 env of   
	S v2      -> appVals (Partial op v1) v2   
	Error msg -> Error msg

appValExpEv (VLamUnit e env) e2 env2 = ev e env
appValExpEv (VLam x   e env) e2 env2 = ev (subst x (App (Var x) Unit) e) (updEnv x (VLamUnit e2 env2) env)
appValExpEv v1 e2 env = Error ( (show v1)                        
	++ " cannot be applied to an argument" )

----------------------------------------------------------------
-- Function for evaluating an expression with no bindings or
-- variables to a value.

ev0 :: Exp -> Error Val
ev0 Unit   = S VUnit
ev0 Nil     = S VNil
ev0 (N n)   = S (VN n)
ev0 (B b)   = S (VB b)
ev0 (Op op) = S (VOp op)

ev0 (App e1 e2) = case (ev0 e1) of 
	Error err -> Error err
	S v1 -> case (ev0 e2) of
		Error err -> Error err
		S v2 -> appVals v1 v2

ev0 (If e1 e2 e3) = case (ev0 e1) of
    S (VB c)  -> if c then ev0 e2 else ev0 e3
    S _       -> Error "'if' condition not a boolean"
    Error err -> Error err

--Problem Set 7 Problem 2 Part B
ev0 (Var x) = Error ( "ev0 should not encounter a variable,"        
	++ "substitution has not been performed"              
	++ "properly")
	
ev0 (LamUnit e) = S $ VLamUnit e emptyEnv
ev0 (Lam xs e)   = S $ VLam xs e emptyEnv

ev0 (Let ((x,e):xs) be) = case ev0 e of
    Error err -> Error err
    S v       -> ev0 (subst x e (Let xs be))

ev0 (Let [] be) = ev0 be

----------------------------------------------------------------
-- Function for evaluating an expression to a value. Note the
-- need for an environment to keep track of variables.

ev :: Exp -> Env Val -> Error Val
ev Unit env = S VUnit
ev Nil env = S VNil 
ev (N n) env = S (VN n)
ev (B b) env = S (VB b)
ev (Op op) env = S (VOp op)
ev (Var x) env =
  case (findEnv x env) of
    Just x' -> S x'
    Nothing -> Error $ "unbound variable: " ++ x

ev (App e1 e2) env =
  case (ev e1 env) of
    Error err -> Error err
    S v1 -> appValExpEv v1 e2 env

ev (Lam x e) env = S (VLam x e env)
ev (LamUnit e) env = S (VLamUnit e env)
	
ev (If e1 e2 e3) env =
  case (ev e1 env) of
    S (VB c)  -> if c then ev e2 env else ev e3 env
    S _       -> Error "'if' condition not a boolean"
    Error err -> Error err
	
ev (Let ((x,e):xs) be) env =
  if not (fv x e) then
    ev (subst x (App (Var x) Unit) (Let xs be)) 
       (updEnv x (VLamUnit e env) env)
  else
    let thunk = VLamUnit (App fixExp (Lam x e)) env
    in 
       ev (subst x (App (Var x) Unit) (Let xs be)) 
          (updEnv x thunk env)
		  
ev (Let [] e) env = ev e env