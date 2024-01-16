{-# LANGUAGE DataKinds #-}

import System.IO
import Language.Haskell.TH (safe)
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))
import Distribution.Compat.ResponseFile (expandResponse)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr

eval :: Expr -> Maybe Int

eval (Val n) = return n
eval (Add x y) = do
    n <- eval x
    m <- eval y
    return (n + m)
eval (Mul x y) = do
    n <- eval x
    m <- eval y
    return (n * m)
eval ( Sub x y) = do
    n <- eval x
    m <- eval y
    return (n - m)
eval (Div x y) = do
    n <- eval x
    m <- eval y
    safeDiv n m
-- Path: test.hs
--
safeDiv :: Int -> Int -> Maybe Int

safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

valtoInt :: Expr -> Int
valtoInt (Val n) = n
valtoInt _ = error "valtoInt can only be applied to Val expressions"

maybetoInt :: Maybe Int -> Int

maybetoInt Nothing = 0
maybetoInt (Just n) = n

main :: IO ()
main = do
    print (maybetoInt (eval (Mul (Val 8) (Val 6))))


