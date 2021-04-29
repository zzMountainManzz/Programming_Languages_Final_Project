-- Programing Lang Final Project
-- Jacob Villemagne, Luke Patterson, Wolfe Bowman 

type DName     = String
type CName     = String
type FName     = String
type RetType   = String
type PType     = String
type PVar      = String
type Vars      = String
type VType     = String
type VName     = String
type OType     = String
type OName     = String
type Arguments = String

type Program = [Decs]

data Decs = DDec (DName, [Statements], [Funcs]) -- driver declaration
          | CDec (CName, [Statements], [Funcs]) -- class declarations

data Statements = Assign VType Vars (Either AExpr BExpr) -- variable assignments
            | Declare CVars -- class level variables
            | Print AExpr -- print statements
            | Ret AExpr -- return statement
            | ODec Obj -- object instantiation

type Obj = (OType, OName, [Arguments])

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


-- layoutChars = "\t\n"
-- layout :: Char -> Bool
-- layout c = c `elem` layoutChars

-- reCommandChars :: String -> String
-- reCommandChars (x:xs) | layout x = reCommandChars xs
--                       | otherwise = x:reCommandChars xs

-- lexer :: String -> [Decs]
-- lexer str | getWord' str == "public class main" = fst(mainReader str):lexer (snd(mainReader str))
--           | getWord' str == "class" = fst(classReader str):lexer (snd(classReader str))
--          | getWord' str == "public" =  fst(functReader str):lexer (snd(functReader str))
--          | getWord' str == "" 

-- constructorReader :: String -> ()

-- mainReader :: String -> (DDec, String)--takes string returns ddec and return string
-- mainReader str = 

--functionReader :: String -> ()


------------------------------------------------------------------------------------
cDriver :: Decs -> String
cDriver (DDec (x, y, z)) = "int main() {" ++ statementLister y ++ "}"
cDriver (CDec (name, states, functs)) = "struct " ++ name ++ "{" ++ statementLister states ++ concat (functToStr functs)

--type Funcs = (RetType, FName, [Params], [Statements])
functToStr :: [Funcs] -> [String]
functToStr [(rettype, name, params, states)] = 

--(DName, [Statements], [Funcs])
-- data Statements = Assign VType Vars (Either AExpr BExpr) -- variable assignments
--             | Declare CVars -- class level variables
--             | Print AExpr -- print statements
--             | Ret AExpr -- return statement
--             | ODec Obj -- object instantiation
statementLister :: [Statements] -> String
statementLister [(Assign x y (Left z))] = x ++ " " ++ y ++ " " ++ "=" ++ " " ++ aExToC z ++ "; "
statementLister ((Assign x y (Left z)):xs) = x ++ " " ++ y ++ " " ++ "=" ++ " " ++ aExToC z ++ "; "++ statementLister xs

aExListToC :: [AExpr] -> String
aExListToC [x] = aExToC x
aExListToC (x:xs) = aExToC x ++ "," ++ aExListToC xs

aExToC :: AExpr -> String
aExToC (Var x) = x
aExToC (Const x) = show x
aExToC (Add x y) = aExToC x ++ " + " ++ aExToC y 
aExToC (Sub x y) = aExToC x ++ " - " ++ aExToC y
aExToC (Div x y) = aExToC x ++ " / " ++ aExToC y
aExToC (FCall x y) = callBuilder x y

callBuilder :: Funcs -> [AExpr] -> String 
callBuilder (_,y,_,_) [a] = y ++ "(" ++ aExToC a ++ ")"
callBuilder (_,y,_,_) (a:as) = y ++ "(" ++ aExToC a ++ "," ++ aExListToC as ++ ")"
---------------------------------------------------------------------------------------

--import System.IO  
--import Control.Monad


-- main :: IO ()
-- main = do
--         let list = []
--         wuTang <- openFile "exmapleFile.txt" ReadMode
--         GhostfaceKillah <- hGetLine exmapleFile
--         OlDirtyBastard <- firstLine                     -- printStringLine
--         hClose exmapleFile
--         putStrln "Wu-tang assembled"














-- data Decs = DDec (DName, [Statements], [Funcs]) -- driver declaration
--         | CDec (CName, [Statements], [Funcs]) -- class declarations
states = [Assign "Int" "X" (Left (Const 1))]
functs :: [Funcs]
functs = []

test1 = DDec ("Student", states, functs)

concreteTest = cDriver test1
