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

data Ops = AddOp | SubOp | MulOp | DivOp | EqlOp | AssignOp | DotOp | LesserOp

data Types = Str | Int | Dble | Lng | Chr | Shrt | Bllean | Bte | Flt

data Token = Class | MainM | VSym String | Csym Integer | LPar | RPar | LBra |
             RBra | Semi | Com | Coln | Qut | Brac | Keyword String | Type Types |
             Op Ops | Err | PA AExpr

classify :: String -> Token
classify "(" = LPar
classify ")" = RPar
classify "{" = LBra
classify "}" = RBra
classify ";" = Semi
classify "," = Com
classify ":" = Coln
classify "\"" = Qut
classify "[]" = Brac
------------ Class stuff ------------
--also fuck having 40 types----------
--class stuff will be known as keywords(look at classifyT for ex)--
classify s@("public")  = Keyword s
classify s@("private") = Keyword "public"--for simplicity
classify s@("class")   = Keyword s
classify s@("main")    = Keyword s
classify s@("new")     = Keyword s
classify s@("static")  = Keyword s
classify s@("args")    = Keyword s
classify s@("void")    = Keyword s
classify s@("this")    = Keyword s
classify s@("return")  = Keyword s
classify s@("system")  = Keyword s
classify s@("out")     = Keyword s
classify s@("print")   = Keyword s
---------- Data Types ------------
classify "String"  = Type Str
classify "int"     = Type Int
classify "double"  = Type Dble
classify "long"    = Type Lng
classify "short"   = Type Shrt
classify "char"    = Type Chr
classify "boolean" = Type Bllean
classify "byte"    = Type Bte
classify "float"   = Type Flt
---------- Operators -------------
classify "-"         = Op SubOp
classify "+"         = Op AddOp
classify "*"         = Op MulOp
classify "\\"        = Op DivOp
classify "=="        = Op EqlOp
classify "="         = Op AssignOp
classify "."         = Op DotOp
classify "<"         = Op LesserOp
classify s | isCSym s = CSym (read s)
classify (x:xs) = VSym (x:xs)

classifyT :: [Token] -> [Token]
classifyT [] = []
classifyT (Keyword "public":Keyword "class":xs) = Class : classifyT xs
classifyT (Keyword "public":Keyword "static":Keyword "void":Keyword "main"
           :LPar:(Type Str):Brac:Keyword "args":xs) = MainM : classifyT xs
classifyT (x:xs)                                      = x : classifyT xs

isCSym :: String -> Bool
isCSym "" = False
isCSym (x:xs) = isDigit x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
        q2 ys = all isDigit ys

preproc :: String -> String
preproc [] = []
preproc ('(':xs)                         = " ( " ++ preproc xs
preproc (')':xs)                         = " ) " ++ preproc xs
preproc ('{':xs)                         = " { " ++ preproc xs
preproc ('}':xs)                         = " } " ++ preproc xs
preproc (';':xs)                         = " ; " ++ preproc xs
preproc (',':xs)                         = " , " ++ preproc xs
preproc (':':xs)                         = " : " ++ preproc xs
preproc ('\"':xs)                        = " \" " ++ preproc xs
preproc ('[':']':xs)                     = " [] " ++ preproc xs
preproc ('p':'u':'b':'l':'i':'c':xs)     = " public " ++ preproc xs
preproc ('p':'r':'i':'v':'a':'t':'e':xs) = " private " ++ preproc xs
preproc ('c':'l':'a':'s':'s':xs)         = " class " ++ preproc xs
preproc ('m':'a':'i':'n':xs)             = " main " ++ preproc xs
preproc ('n':'e':'w':xs)                 = " new " ++ preproc xs
preproc ('s':'t':'a':'t':'i':'c':xs)     = " static " ++ preproc xs
preproc ('a':'r':'g':'s':xs)             = " args " ++ preproc xs
preproc ('v':'o':'i':'d':xs)             = " void " ++ preproc xs
preproc ('t':'h':'i':'s':xs)             = " this " ++ preproc xs
preproc ('r':'e':'t':'u':'r':'n':xs)     = " return " ++ preproc xs
preproc ('S':'y':'s':'t':'e':'m':xs)     = " System " ++ preproc xs
preproc ('o':'u':'t':xs)                 = " out " ++ preproc xs
preproc ('p':'r':'i':'n':'t':xs)         = " print " ++ preproc xs
preproc ('S':'t':'r':'i':'n':'g':xs)     = " String " ++ preproc xs
preproc ('i':'n':'t':xs)                 = " int " ++ preproc xs
preproc ('d':'o':'u':'b':'l':'e':xs)     = " double " ++ preproc xs
preproc ('l':'o':'n':'g':xs)             = " long " ++ preproc xs
preproc ('s':'h':'o':'r':'t':xs)         = " short " ++ preproc xs
preproc ('c':'h':'a':'r':xs)             = " char " ++ preproc xs
preproc ('b':'o':'o':'l':'e':'a':'n':xs) = " boolean " ++ preproc xs
preproc ('b':'y':'t':'e':xs)             = " byte " ++ preproc xs
preproc ('f':'l':'o':'a':'t':xs)         = " float " ++ preproc xs
preproc ('-':xs)                         = " - " ++ preproc xs
preproc ('+':xs)                         = " + " ++ preproc xs
preproc ('*':xs)                         = " * " ++ preproc xs
preproc ('/':'\\':xs)                    = " /\\ " ++ preproc xs -----CHECK THIS
preproc ('=':'=':xs)                     = " == " ++ preproc xs
preproc ('=':xs)                         = " = " ++ preproc xs
preproc ('.':xs)                         = " . " ++ preproc xs
preproc ('<':xs)                         = " < " ++ preproc xs
preproc (x:xs)                           = x : preproc xs

lexer :: String -> [Token]
lexer x = map classify (words (preproc x))

--data Token = Class | MainM | VSym String | Csym Integer  LPar | RPar | LBra |
--             RBra | Semi | Com | Coln | Qut | Brac | PP | Clss | Min | Nw | St |
--             Arg | Vod | Ths | Rtrn | Systm | Out | Prnt | Type Types | Op Ops | Err

sr :: [Token] -> [Token] -> [Token]
sr (VSym v : stack) input                           = sr (PA (Var v) : stack) input
sr (CSym c : stack) input                           = sr (PA (Const c) : stack) input
sr (PA e2 : BOp AddOp : PA e1 : stack) input        = sr (PA (Add e1 e2) : stack) input
sr (PA e2 : BOp SubOp : PA e1 : stack) input        = sr (PA (Sub e1 e2) : stack) input
sr (PA e2 : BOp MulOp : PA e1 : stack) input        = sr (PA (Mul e1 e2) : stack) input
sr (PA e2 : BOp DivOp : PA e1 : stack) input        = sr (PA (Div e1 e2) : stack) input
sr (MainM:stack) input = sr ( : stack) input
sr (Class:(VSym Str):stack) input = sr ( : stack) input


sr (Keyword "nop" : stack) input                    = sr (PI Nop : stack) input
sr stack (i:input)                                  = sr (i:stack) input
sr stack []                                         = stack


parser :: [Token] -> [DDec]
parser input = case sr [] input of
         [PI s] -> s
         l      -> error ("bad parse" ++ show l)

------------------------------------------------------------------------------------
cDriver :: Decs -> String
cDriver (DDec (x, y, z)) = "int main() {" ++ statementLister y ++ functionLister z ++ "}"
cDriver (CDec (x, y, z)) = "struct " ++ x ++ " {" ++ statementLister y ++ "}; " ++ funcyListBuilder x z 

statementLister :: [Statements] -> String
statementLister [Assign x y (Left z)] = dataConv x ++ " " ++ strCheck x y ++ " " ++ "=" ++ " " ++ aExToC z ++ "; "
statementLister ((Assign x y (Left z)):xs) = dataConv x ++ " " ++ y ++ " " ++ "=" ++ " " ++ aExToC z ++ "; "++ statementLister xs
statementLister [ReAssign x (Left z)] = x ++ " " ++ " = " ++ " " ++ aExToC z ++ "; "
statementLister ((ReAssign x (Left z)):xs) = x ++ " " ++ " = " ++ " " ++ aExToC z ++ "; " ++ statementLister xs
statementLister [Declare (x,y)] = dataConv x ++ " " ++ y ++ "; "
statementLister ((Declare (x,y)):xs) = dataConv x ++ " " ++ y ++ "; " ++ statementLister xs
statementLister [Print x] = "printf(" ++ aExToC x ++ "); "
statementLister ((Print x):xs) = "printf(" ++ aExToC x ++ "); " ++ statementLister xs
statementLister [Ret x] = "return " ++ aExToC x ++ ";"
statementLister ((Ret x):xs) = "return " ++ aExToC x ++ ";" ++ statementLister xs
statementLister [OInst (x,y,z)] = "struct " ++ x ++ " " ++ y ++ " = " ++ x ++ "_" ++ x ++ "(" ++ argsLister z ++ "); " 
statementLister ((OInst (x,y,z)):xs) = "struct " ++ x ++ " " ++ y ++ " = " ++ x ++ "_" ++ x ++ "(" ++ argsLister z ++ "); " ++ statementLister xs
{- When making [AExpr] we need to store arguments like "&objectReference", *****************this may not work ): 
   example: student_setAge(&Jacob,student_getAge(Wolfe) - student_getAge(Luke)) would look like 
   [Var "&Jacob", Sub (FCall (int, student_getAge,_,_) ["Wolfe"]) (FCall (int, student_getAge,_,_) ["Luke"])]-}
statementLister [FCall2 x y] = callBuilder x y
statementLister ((FCall2 x y):xs) = callBuilder x y ++ statementLister xs

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
toParen :: AExpr -> Bool
toParen (Var x) = False
toParen (Const x) = False
toParen _ = True;
putParen :: AExpr -> String
putParen z = if toParen z then "(" ++ aExToC z ++ ")" else aExToC z
-- For converting individual AExpr's
aExToC :: AExpr -> String
aExToC (Var x) = x
aExToC (Const x) = show x
aExToC (Add x y) = putParen x ++ " + " ++ putParen y
aExToC (Sub x y) = putParen x ++ " - " ++ putParen y
aExToC (Div x y) = putParen x ++ " / " ++ putParen y
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
paramLister :: [Params] -> String
paramLister [] = ""
paramLister ((varType, varName):xs) | xs /= []  = dataConv varType ++ " " ++ varName ++ "," ++ paramLister xs
                                    | otherwise = dataConv varType ++ " " ++ varName

functionLister :: [Funcs] -> String
functionLister ((return, name, params, states):xs) = dataConv return ++ " " ++ name ++ "(" ++ paramLister params ++ ")" ++ "{" ++ statementLister states ++ "}"
----------------------------------------------------------------------------------------
-- Pass name and list of functions to be built
funcyListBuilder :: CName -> [Funcs] -> String
funcyListBuilder x [y] = funcyBuilder x y
funcyListBuilder x (y:ys) = funcyBuilder x y ++ funcyListBuilder x ys

-- Build functions and constructor for simulated class
{- When saving statements in constructor we need to save the variable names with obj.name like newStudent.name -}
funcyBuilder :: CName -> Funcs -> String
funcyBuilder n (w,x,y,z) | n == w && n == x = "struct " ++ n ++ " " ++ n ++ "_" ++ n ++ "(" ++ paramLister y ++ ")" ++ "{" ++
                                    "struct " ++ n ++ " new" ++ n ++ "; " ++ statementLister z ++ " return " ++ " new" ++ n ++ ";"
                         | otherwise = dataConv w ++ " " ++ x ++ "(" ++ paramLister y ++ ")" ++ "{" ++ statementLister z ++ "}"

-------------------------------TEST-----------------------------------------------------
states1 = [Assign "int" "X" (Left (Const 1)), 
           ReAssign "X" (Left (Add (Const 60) (Sub (Const 10) (Var "X"))))]
functs1 :: [Funcs]
functs1 = [("int", "addOne", [("int","x")],
          [ReAssign "X" (Left (Add (Var "x") (Const 1)))])]

states2 = [Assign "int" "Y" (Left (Const 5))]

functs2 = [("int", "getY", [], [Ret (Var "Y")])]

test1 = DDec ("main", states1, functs1)
test2 = CDec ("Student", states2, functs2)

concreteTest1 = cDriver test1
concreteTest2 = cDriver test2
-------------------------------TEST-----------------------------------------------------
