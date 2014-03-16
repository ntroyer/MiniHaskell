----------------------------------------------------------------
-- Computer Science 320 (Fall, 2012)
-- Concepts of Programming Languages
--
-- Assignment 8
--   Ty.hs
--   Nat Troyer

----------------------------------------------------------------
-- Syntax for Types for the mini-Haskell Interpreter

module Ty (typeCheck, AnnotVal(AnnotVal)) 
  where

import Exp (Exp(..), Oper(..))
import Err
import Env
import Val

data Ty = TyUnit
        | TyVar String
        | TyBool
        | TyInt
        | TyBoolList
        | TyIntList
        | Arrow Ty Ty
  deriving Eq

-- Annotated values, for printing in Main
data AnnotVal = AnnotVal Val Ty
instance Show AnnotVal where
  show (AnnotVal v t) = (show v) ++ " :: " ++ (show t)

-- Useful function for transforming environment (used for
-- applying substitutions to environment).
mapEnv :: (a -> b) -> Env a -> Env b
mapEnv s ((x, t):xts) = (x, s t):(mapEnv s xts)
mapEnv s [] = []

----------------------------------------------------------------
-- Canonical form

data PolyTy = ForAll [String] Ty
showVars (v:vs) = v ++ " "
showVars [v] = v
showVars [] = ""
instance Show PolyTy where
  show (ForAll [] t) = show t
  show (ForAll vs t) = "forall " ++ (showVars vs) ++ "." ++ (show t)

--canon :: Ty -> PolyTy
--Assignment 8, Problem #4 (b) Not Yet Implemented

--freevars :: Ty -> [String]
--Assignment 8, Problem #4 (a) Not Yet Implemented"]

----------------------------------------------------------------
-- This function is exported to the Main module.

-- For testing purposes, you may want to replace the
-- body of typeCheck with a call (ty0 e)

typeCheck :: Exp -> Error Ty
typeCheck e =
  case (ty emptyEnv freshTyVars e) of
    Error msg -> Error msg
    S (t, s, fv) -> S $ s t -- We apply the substitution before
                            -- returning the type.

----------------------------------------------------------------
-- Type-checking Algorithm

tyOp :: Oper -> Ty
tyOp Plus =  Arrow TyInt (Arrow TyInt TyInt)
tyOp Times = Arrow TyInt (Arrow TyInt TyInt)
tyOp Equal = Arrow TyInt (Arrow TyInt TyBool)
tyOp And   = Arrow TyBool (Arrow TyBool TyBool)
tyOp Or    = Arrow TyBool (Arrow TyBool TyBool)
tyOp Not   = Arrow TyBool TyBool
tyOp Head  = Arrow (TyIntList) TyInt
--tyOp Head  = Arrow (TyBoolList) TyBool
tyOp Tail  = Arrow (TyIntList) (TyIntList)
--tyOp Tail  = Arrow (TyBoolList) (TyBoolList)
tyOp Cons  = Arrow TyInt (Arrow (TyIntList) (TyIntList))
--tyOp Cons  = Arrow TyBool (Arrow (TyBoolList) (TyBoolList))

ty0 :: Exp -> Error Ty
ty0 Unit = S TyUnit
ty0 Nil = S $ TyIntList
ty0 (N _) = S TyInt
ty0 (B _) = S TyBool
ty0 (Op op) = S (tyOp op)
ty0 (If e1 e2 e3) =
  case (ty0 e1, ty0 e2, ty0 e3) of
    (Error msg, _, _) -> Error msg
    (_, Error msg, _) -> Error msg
    (_, _, Error msg) -> Error msg
    (S t1, S t2, S t3) -> 
      if (t1 == TyBool) && (t2 == t3) then
        S t2
      else
        Error "'if' type mismatch"
		
ty0 (App e1 e2) =
  case (ty0 e1, ty0 e2) of
    (Error msg, _) -> Error msg
    (_, Error msg) -> Error msg
    (S (Arrow t1 t2), S t1') ->
      if (t1 == t1') then
        S t2
      else
        Error "function applied to argument of wrong type."
    (S _, _) -> Error "non-function applied to argument."

ty :: Env Ty -> FreshVars -> Exp -> Error (Ty, Subst, FreshVars)

ty gamma fvs Unit			= S (TyUnit, idsubst, fvs)
ty gamma fvs Nil       = S (TyIntList, idsubst, fvs)
ty gamma (fv:fvs) (Op Cons) = S (Arrow fv (Arrow (TyIntList) (TyIntList)), idsubst, fvs)
--ty gamma (fv:fvs) (Op Cons) = S (Arrow fv (Arrow (TyBoolList) (TyBoolList)), idsubst, fvs)
ty gamma (fv:fvs) (Op Head) = S (Arrow (TyIntList) fv, idsubst, fvs)
--ty gamma (fv:fvs) (Op Head) = S (Arrow (TyBoolList) fv, idsubst, fvs)
ty gamma (fv:fvs) (Op Tail) = S (Arrow (TyIntList) (TyIntList), idsubst, fvs)
--ty gamma (fv:fvs) (Op Tail) = S (Arrow (TyBoolList) (TyBoolList), idsubst, fvs)
ty gamma fvs (N n)          = S (TyInt, idsubst, fvs)
ty gamma fvs (B b)          = S (TyBool, idsubst, fvs)
ty gamma fvs (Op Plus)      = S (Arrow TyInt (Arrow TyInt TyInt), idsubst, fvs)
ty gamma fvs (Op Times)     = S (Arrow TyInt (Arrow TyInt TyInt), idsubst, fvs)
ty gamma fvs (Op Equal)     = S (Arrow TyInt (Arrow TyInt TyBool), idsubst, fvs)
ty gamma fvs (Op And)       = S (Arrow TyBool (Arrow TyBool TyBool), idsubst, fvs)
ty gamma fvs (Op Or)        = S (Arrow TyBool (Arrow TyBool TyBool), idsubst, fvs)
ty gamma fvs (Op Not)       = S (Arrow TyBool TyBool, idsubst, fvs)

-- Base cases missing
-- Assignment 8, Problem #4 (a) Not Yet Implemented

ty gamma fvs (Var x) =
  case (findEnv x gamma) of
    Nothing -> Error $ "unbound variable " ++ x
    Just t  -> S (t, idsubst, fvs)

ty gamma fvs (If e1 e2 e3) =
  case (tys gamma fvs [e1, e2, e3]) of
    Error msg -> Error msg
    S ([t1, t2, t3], s1, fvs') -> 
		case (unify (t1) (TyBool)) of
			Error msg -> Error msg
			S (s2) -> case (unify (t2) (t3)) of
				Error msg -> Error msg
				S (s3) -> S(t2, s3 `o` s2 `o` s1, fvs')
					

ty gamma fvs (App e1 e2) =
  case (tys gamma fvs [e1, e2]) of
    Error msg -> Error msg
    S ([t1, t2], s, (fv:fvs'')) ->
      case (unify (s t1) (Arrow (s t2) fv)) of
        Error msg -> Error msg
        S s'' -> S (s(s'' fv), s `o` s'', fvs'')

ty gamma (fv:fvs') (Lam x e) =
   case (ty (updEnv x fv gamma) fvs' e) of
		Error msg -> Error msg
		S (t, s, fvs'') -> S (Arrow fv t, s, fvs'')

ty gamma fvs (LamUnit e) =
   case (ty gamma fvs e) of
		Error msg -> Error msg
		S (t, s, fvs') -> S (Arrow TyUnit t, s, fvs')

ty gamma (fv:fvs') (Let ((x,e):xes) be) =
  case (ty (updEnv x fv gamma) fvs' e) of
    Error msg       -> Error msg
    S (t, s, fvs'') -> ty (updEnv x (s t) gamma) fvs'' (Let xes be)
ty gamma fvs (Let [] be) = ty gamma fvs be

ty gamma fvs e = Error "cannot infer type"

-- This function infers the types of a list of expressions,
-- accumulating the substitutions and returning their
-- composition along with the list of types.
tys :: Env Ty -> FreshVars -> [Exp] -> Error ([Ty], Subst, FreshVars)
tys gamma fvs (e:es) =
  case (tys gamma fvs es) of
    Error msg -> Error msg
    S (ts, s, fvs') ->
      case (ty (mapEnv s gamma) fvs' e) of
        Error msg -> Error msg
        S (t, s', fvs'') -> S (t:ts, \x -> s (s' x), fvs'')
tys gamma fvs [] = S ([], idsubst, fvs)

----------------------------------------------------------------
-- Type Unification

unify :: Ty -> Ty -> Error Subst

unify t (TyVar x) = S (subst x t)
unify (TyVar x) t = S (subst x t)
unify (Arrow t1 t2) (Arrow t1' t2') = case (unify t1 t1') of
	Error msg -> Error msg
	S s1 -> case (unify t2 t2') of
		Error msg2 -> Error msg2
		S s2 -> S (o (s2) (s1))
	
unify t t' = if t == t' then S (idsubst) else Error "Unify error"

----------------------------------------------------------------
-- Type Variable Substitutions

type Subst = Ty -> Ty

--placeHolder :: String -> Subst
--placeHolder s = \t->TyVar s

idsubst :: Subst
idsubst = \x -> x

o :: Subst -> Subst -> Subst
t1 `o` t2 = \x -> t1 (t2 x)

subst :: String -> Ty -> Subst
subst s t TyBool = TyBool
subst s t (Arrow t1 t2) = Arrow (subst s t t1) (subst s t t2)
subst s t (TyVar x)       = if (s == x) then t else TyVar x
subst s t t'              = t'

----------------------------------------------------------------
-- Infinite List of Fresh Type Variables

type FreshVars = [Ty]
freshTyVars :: FreshVars
freshTyVars = fvs 0
  where fvs n = TyVar ("t" ++ (show n)):(fvs (n+1))

----------------------------------------------------------------
-- Printing functions for Syntax of Types

showTy TyUnit      = "()"
showTy (TyVar s)   = s
showTy TyBool      = "Bool"
showTy TyInt       = "Int"
showTy TyBoolList  = "[Bool]"
showTy TyIntList   = "[Int]"
-- If the left argument of an arrow is an arrow, we need to
-- add parentheses to make sure the type is not ambiguous.
showTy (Arrow (Arrow t1 t2) t3) =
   "(" ++ (showTy (Arrow t1 t2)) ++ ") -> " ++ (showTy t3)
showTy (Arrow t1 t2) =
   (showTy t1) ++ " -> " ++ (showTy t2)

instance Show Ty where
  show = showTy
