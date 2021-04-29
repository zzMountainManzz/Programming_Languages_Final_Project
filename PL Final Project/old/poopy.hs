-- Programing Lang Final Project
-- Jacob Villemagne, Luke Patterson, Wolfe Bowman 

type DName   = String
type CName   = String
type FName   = String
type RetType = String
type PType   = String
type PVar    = String
type Vars    = String
type VTpe    = String
type VName   = String

type Program = [Decs]

data Decs = DDec (DName, [Statements], [Funcs]) -- driver declaration
        | CDec (CName, [Statements], [Funcs]) -- class declarations

data Statements = Assign Vars (Either AExpr BExpr) -- variable assignments
            | Declare CVars -- class level variables
            | Print AExpr -- print statements
            | Ret AExpr -- return statement

type CVars = (VType, VName)

type Funcs = (RetType, FName, [Params], [Statements])

type Params = (PType, PVar)

data AExpr = Var Vars | Const Integer
                      | Add AExpr AExpr | Sub AExpr AExpr
                      | Mul AExpr AExpr | Div AExpr AExpr
                      | FCall Funcs [AExpr] -- function call with list of args

data BExpr = TT | FF -- the true and false constants
                | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
                | Eql AExpr AExpr -- equality of arithmetic expressions
                | Lt AExpr AExpr -- true if the first is less than the second
                | Gt AExpr AExpr -- greater than
                | GtE AExpr AExpr -- greater than equal to
                | LtE AExpr AExpr -- less than equal to


layoutChars = "\t\n"
layout :: Char -> Bool
layout c = c `elem` layoutChars

reCommandChars :: String -> String
reCommandChars (x:xs) | layout x = reCommandChars xs
                      | otherwise = x:reCommandChars xs

getWord' :: String -> String
getWord' [] = []
getWord' (x:xs) | x `elem` halts = []
               | otherwise = x:getWord' xs

--from the hellscape
getScuffed :: String -> (String, String)
getScuffed [] = ([], [])
getScuffed (x:xs) | x `elem` halts = ([], xs)
                  | otherwise = (x:ys, zs)
                    where (ys, zs) = getScuffed xs


lexer :: String -> [Decs]
lexer str | getWord' str == "public class main" = fst(mainReader str):lexer (snd(mainReader str))
          | getWord' str == "class" = fst(classReader str):lexer (snd(classReader str))
          | getWord' str == "public" =  fst(functReader str):lexer (snd(functReader str))

mainReader :: String -> (DDec, String)--takes string returns ddec and return string
mainReader str = 


import System.IO
import Control.Monad


main :: IO ()
main = do
        let list = []
        wuTang <- openFile "exmapleFile.txt" ReadMode
        GhostfaceKillah <- hGetLine exmapleFile
        OlDirtyBastard <- firstLine                     -- printStringLine
        hClose exmapleFile
        putStrln "Wu-tang assembled"