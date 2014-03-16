----------------------------------------------------------------
-- Computer Science 320 (Fall, 2012)
-- Concepts of Programming Languages
--
-- Assignment 6
--   Env.hs
--   Nat Troyer

----------------------------------------------------------------
-- Environments

module Env (emptyEnv, updEnv, findEnv, Env)
  where

type Env a = [(String, a)]

emptyEnv :: Env a
emptyEnv = []

updEnv :: String -> a -> Env a -> Env a
updEnv a b xs = [(a,b)] ++ xs

findEnv :: String -> Env a -> Maybe a
findEnv x [] = Nothing
findEnv x (y:ys)
	|x == (fst y) = Just (snd y)
	|otherwise = findEnv x ys