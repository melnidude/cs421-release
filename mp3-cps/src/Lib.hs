--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush, nativeNewline)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) (\v -> k (n * v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [] ke ko = ke 0
evenoddk [x] ke ko 
    | even x = ke x
    | otherwise = ko x
evenoddk (x:xs) ke ko 
    | even x = evenoddk xs (\v -> ke (v + x)) ko
    | otherwise = evenoddk xs ke (\v -> ko (v + x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (LamExp s body) = isSimple body
isSimple (IfExp a b c) = isSimple a && isSimple b && isSimple c
isSimple (OpExp s a b) = isSimple a && isSimple b
isSimple (AppExp _ _) = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)


--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f arg) k n
    | isSimple arg = (AppExp (AppExp f arg) k, n)
    | otherwise = 
        let (v, n1) = gensym n
            result = LamExp v (AppExp (AppExp f (VarExp v)) k)
            (arg2, n2) = cpsExp arg result n1 
        in (arg2, n2)
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n
    | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2), n) --case 1

    | isSimple e1 = --case 2
        let (v1, n1) = gensym n
            result = LamExp v1 (AppExp k (OpExp op e1 (VarExp v1)))
            (e2_transformed, n2) = cpsExp e2 result n1 
        in (e2_transformed, n2)

    | isSimple e2 = --case 3
        let (v2, n1) = gensym n
            result = LamExp v2 (AppExp k (OpExp op (VarExp v2) e2))
            (e1_transformed, n2) = cpsExp e1 result n1 
        in (e1_transformed, n2)

    | otherwise = --case 4
        let (v1, n1) = gensym n
            result = LamExp v1 (
                let (v2, n2) = gensym n1
                    result2 = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
                    (e2_transformed, n3) = cpsExp e2 result2 n2
                in e2_transformed)
            (e1_transformed, n4) = cpsExp e1 result n1
        in (e1_transformed, n4)
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp a b c) k n
    | isSimple a = 
        let (b_t, n1) = cpsExp b k n
            (c_t, n2) = cpsExp c k n1
        in (IfExp a b_t c_t, n2)

    | otherwise = 
        let (v, n1) = gensym n
            (b_t, n2) = cpsExp b k n1
            (c_t, n3) = cpsExp c k n2
            result = LamExp v (IfExp (VarExp v) b_t c_t)
            (a_t, n4) = cpsExp a result n3
        in (a_t, n4)
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f args body) =
    let k = "k"
        (body_t, _) = cpsExp body (VarExp k) 0
    in Decl f (args ++ [k]) body_t
