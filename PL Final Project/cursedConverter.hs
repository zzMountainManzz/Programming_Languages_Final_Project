-- Programing Lang Final Project
-- Jacob Villemagne, Luke Patterson, Wolfe Bowman 

type DName     = String
type CName     = String
type FName     = String
type RetType   = String
type PType     = String
type PVar      = String
type Vars      = String
type VTpe      = String
type VName     = String
type OType     = String
type OName     = String
type Arguments = String


type Program = [Decs]

data Decs = DDec (DName, [Statements], [Funcs]) -- driver declaration
        | CDec (CName, [Statements], [Funcs]) -- class declarations

data Statements = Assign VType Vars (Either AExpr BExpr) -- variable assignments
            | ReAssign Vars (Either AExpr BExpr) -- reassign variable
            | Declare CVars -- class level variables
            | Print AExpr -- print statements
            | Ret AExpr -- return statement
            | OInst Obj -- object instantiation
            | FCall2 Funcs [AExpr] -- function call

type Obj = (OType, OName, [Arguments])

type CVars = (VType, VName)

type Funcs = (RetType, FName, [Params], [Statements])

type Params = (PType, PVar)

data AExpr = Var Vars | Const Integer
                      | Add AExpr AExpr | Sub AExpr AExpr
                      | Mul AExpr AExpr | Div AExpr AExpr
                      | FCall Funcs [AExpr] -- function call inside statement with list of args

data BExpr = TT | FF -- the true and false constants
                | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
                | Eql AExpr AExpr -- equality of arithmetic expressions
                | Lt AExpr AExpr -- true if the first is less than the second
                | Gt AExpr AExpr -- greater than
                | GtE AExpr AExpr -- greater than equal to
                | LtE AExpr AExpr -- less than equal to


-- layoutChars = "\t\n"
-- layout :: Char -> Bool
-- layout c = c `elem` layoutChars

-- reCommandChars :: String -> String
-- reCommandChars (x:xs) | layout x = reCommandChars xs
--                       | otherwise = x:reCommandChars xs

-- lexer :: String -> [Decs]
-- lexer str | getWord' str == "public class main" = fst(mainReader str):lexer (snd(mainReader str))
--           | getWord' str == "class" = fst(classReader str):lexer (snd(classReader str))
-- --          | getWord' str == "public" =  fst(functReader str):lexer (snd(functReader str))
-- --          | getWord' str == "" 

-- constructorReader :: String -> ()

-- mainReader :: String -> (DDec, String)--takes string returns ddec and return string
-- mainReader str = 

-- functionReader :: String -> ()

-- import System.IO  
-- import Control.Monad


-- main :: IO ()
-- main = do
--         let list = []
--         wuTang <- openFile "exmapleFile.txt" ReadMode
--         GhostfaceKillah <- hGetLine exmapleFile
--         OlDirtyBastard <- firstLine                     -- printStringLine
--         hClose exmapleFile
--         putStrln "Wu-tang assembled"


------------------------------------------------------------------------------------
cDriver :: Decs -> String
cDriver (DDec (x, y, z)) = "int main() {" ++ statementLister y ++ "}"

statementLister :: [Statements] -> String
statementLister [(Assign x y (Left z))] = dataConv x ++ " " ++ strCheck x y ++ " " ++ "=" ++ " " ++ aExToC z ++ "; "
statementLister ((Assign x y (Left z)):xs) = dataConv x ++ " " ++ y ++ " " ++ "=" ++ " " ++ aExToC z ++ "; "++ statementLister xs
statementLister [(ReAssign x (Left z))] = x ++ " " ++ " = " ++ " " ++ aExToC z ++ "; "
statementLister ((ReAssign x (Left z)):xs) = x ++ " " ++ " = " ++ " " ++ aExToC z ++ "; " ++ statementLister xs
statementLister [(Declare (x,y))] = dataConv x ++ " " ++ y ++ "; "
statementLister ((Declare (x,y)):xs) = dataConv x ++ " " ++ y ++ "; " ++ statementLister xs
statementLister [(Print x)] = "printf(" ++ aExToC x ++ "); "
statementLister ((Print x):xs) = "printf(" ++ aExToC x ++ "); " ++ statementLister xs
statementLister [(Ret x)] = "return " ++ aExToC x ++ ";"
statementLister ((Ret x):xs) = "return " ++ aExToC x ++ ";" ++ statementLister xs
statementLister [(OInst (x,y,z))] = "struct " ++ x ++ " " ++ y ++ " = " ++ x ++ "_" ++ x ++ "(" ++ argsLister z ++ "); " 
statementLister ((OInst (x,y,z)):xs) = "struct " ++ x ++ " " ++ y ++ " = " ++ x ++ "_" ++ x ++ "(" ++ argsLister z ++ "); " ++ statementLister xs

-- Check if data type of variable needs to be changed from Java formatting to C
dataConv :: String -> String
dataConv x | x == "String" = "char*"
           | otherwise = x 

-- Check if variable is a string and needs quotation marks
strCheck :: String -> String -> String
strCheck x y | x == "String" = "\"" ++ y ++ "\""
             | otherwise = y

-- List AExpr's in arguments with comma separator
aExListToC :: [AExpr] -> String
aExListToC [x] = aExToC x
aExListToC (x:xs) = aExToC x ++ "," ++ aExListToC xs

-- For converting individual AExpr's
aExToC :: AExpr -> String
aExToC (Var x) = x
aExToC (Const x) = show x
aExToC (Add x y) = aExToC x ++ " + " ++ aExToC y 
aExToC (Sub x y) = aExToC x ++ " - " ++ aExToC y
aExToC (Div x y) = aExToC x ++ " / " ++ aExToC y
aExToC (FCall x y) = callBuilder x y

-- Build a function call with list of AExpr as arguments
callBuilder :: Funcs -> [AExpr] -> String 
callBuilder (_,y,_,_) [a] = y ++ "(" ++ aExToC a ++ ")"
callBuilder (_,y,_,_) (a:as) = y ++ "(" ++ aExToC a ++ "," ++ aExListToC as ++ ")"

-- List arguments in object instantiation (WE SHOULD STORE ARGUMENTS WITH QUOTES IF THEY ARE STRINGS)
argsLister :: [Arguments] -> String 
argsLister [x] = x
argsLister (x:xs) = x ++ ", " ++ argsLister xs
---------------------------------------------------------------------------------------