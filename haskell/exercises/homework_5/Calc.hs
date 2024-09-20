{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Data.Map qualified as M
import ExprT
import Parser
import StackVM qualified as S

-- ********** Exercise 1 **********

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul em1 em2) = eval em1 * eval em2

-- ********** Exercise 2 **********

evalStr :: String -> Maybe Integer
-- evalStr = fmap eval . parseExp Lit Add Mul
evalStr str = case parseExp Lit Add Mul str of
  Nothing -> Nothing
  Just expr -> Just (eval expr)

mapExprT :: (Integer -> Integer) -> ExprT -> ExprT
mapExprT f (Lit n) = Lit (f n)
mapExprT f (Add e1 e2) = Add (mapExprT f e1) (mapExprT f e2)
mapExprT f (Mul e1 e2) = Mul (mapExprT f e1) (mapExprT f e2)

-- ********** Exercise 3 **********

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit :: Integer -> ExprT
  lit = Lit
  add :: ExprT -> ExprT -> ExprT
  add = Add
  mul :: ExprT -> ExprT -> ExprT
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- ********** Exercise 4 **********

instance Expr Integer where
  lit :: Integer -> Integer
  lit = id
  add :: Integer -> Integer -> Integer
  add = (+)
  mul :: Integer -> Integer -> Integer
  mul = (*)

instance Expr Bool where
  lit :: Integer -> Bool
  lit n = n > 0
  add :: Bool -> Bool -> Bool
  add = (||)
  mul :: Bool -> Bool -> Bool
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit :: Integer -> MinMax
  lit = MinMax
  add :: MinMax -> MinMax -> MinMax
  add (MinMax n1) (MinMax n2) = MinMax (max n1 n2)
  mul :: MinMax -> MinMax -> MinMax
  mul (MinMax n1) (MinMax n2) = MinMax (min n1 n2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit :: Integer -> Mod7
  lit n = Mod7 (n `mod` 7)
  add :: Mod7 -> Mod7 -> Mod7
  add (Mod7 n1) (Mod7 n2) = Mod7 ((n1 + n2) `mod` 7)
  mul :: Mod7 -> Mod7 -> Mod7
  mul (Mod7 n1) (Mod7 n2) = Mod7 ((n1 * n2) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- ********** Exercise 5 **********

instance Expr S.Program where
  lit :: Integer -> S.Program
  lit n = [S.PushI n]
  add :: S.Program -> S.Program -> S.Program
  add p1 p2 = p1 ++ p2 ++ [S.Add]
  mul :: S.Program -> S.Program -> S.Program
  mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

testProgram :: Maybe S.Program
testProgram = compile "(3 * -4) + 5"

executeProgram :: Maybe S.Program -> Either String S.StackVal
executeProgram Nothing = Left "Error: Invalid program"
executeProgram (Just program) = S.stackVM program

-- ********** Exercise 6 **********

class HasVars a where
  var :: String -> a

data VarExprT
  = LitV Integer
  | AddV VarExprT VarExprT
  | MulV VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit :: Integer -> VarExprT
  lit = LitV
  add :: VarExprT -> VarExprT -> VarExprT
  add = AddV
  mul :: VarExprT -> VarExprT -> VarExprT
  mul = MulV

instance HasVars VarExprT where
  var :: String -> VarExprT
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var :: String -> M.Map String Integer -> Maybe Integer
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit :: Integer -> (M.Map String Integer -> Maybe Integer)
  lit n _ = Just n

  add :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer)
  add f1 f2 m = case (f1 m, f2 m) of
    (Just n1, Just n2) -> Just (n1 + n2)
    _ -> Nothing

  mul :: (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer) -> (M.Map String Integer -> Maybe Integer)
  mul f1 f2 m = case (f1 m, f2 m) of
    (Just n1, Just n2) -> Just (n1 * n2)
    _ -> Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- withVars [("x", 6)] $ add (lit 3) (var "y")

-- withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
