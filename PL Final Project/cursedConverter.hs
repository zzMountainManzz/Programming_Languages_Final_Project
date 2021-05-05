-- Programing Lang Final Project
-- Jacob Villemagne, Luke Patterson, Wolfe Bowman 
import Data.Char 

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

data Decs = DDec (String, [Statements]) -- driver declaration
        | CDec (CName, Token, [Params], [Funcs]) -- class declarations
  deriving Show

data Statements = Assign VType Vars AExpr -- variable assignments
            | ReAssign Vars String -- reassign variable
            | TReAssign Vars String --reassign but with this. before var
            | Declare CVars -- class level variables
            | Print AExpr -- print statements
            | Ret AExpr -- return statement
            | OInst Obj -- object instantiation
            | FCall2 Funcs [AExpr] -- function call
            | ObjFCall (String, String, AExpr)
            | ObjectPrint (String, String, [Params])
  deriving Show

type Obj = (OType, OName, [Arguments])

type CVars = (Types, VName)

type Funcs = (Types, FName, [Params], [Statements])

type Params = (Types, PVar)

data AExpr = Var Vars | Const Integer
                      | Add AExpr AExpr | Sub AExpr AExpr
                      | Mul AExpr AExpr | Div AExpr AExpr
                      | FCall Funcs [AExpr] -- function call inside statement with list of args
                      | AeObjCall (String, String, [Params])
  deriving Show

data BExpr = TT | FF -- the true and false constants
                | And BExpr BExpr | Or BExpr BExpr | Not BExpr -- boolean operations
                | Eql AExpr AExpr -- equality of arithmetic expressions
                | Lt AExpr AExpr -- true if the first is less than the second
                | Gt AExpr AExpr -- greater than
                | GtE AExpr AExpr -- greater than equal to
                | LtE AExpr AExpr -- less than equal to
  deriving Show

data Ops = AddOp | SubOp | MulOp | DivOp | EqlOp | AssignOp | DotOp | LesserOp
  deriving Show

data Types = Str | Int | Dble | Lng | Chr | Shrt | Bllean | Bte | Flt | Vid
  deriving Show

data Token = Class | MainM | VSym String | Csym Integer | LPar | RPar | LBra |
             RBra | Semi | Com | Coln | Qut | Brac | Keyword String | Type Types |
             Op Ops | Err | PA AExpr | PB BExpr | Declaration CVars | Assignment AExpr |
             States Statements | ConPar [Arguments] |
             FuncPar [Params] | AlmostFunc (Types, String, [Params]) | VarDec Params |
             ClassName String | CFuncHeader (String, [Params]) | 
             ObJCall (String, String, [Params]) | TPrint | APrint Token | State Token |
             ObjCallAe (String, String, AExpr) | StateList [Statements] | VarList [Params] | ClassCall |
             FullFunct Funcs | FullCFunct (String, [Params], [Statements]) | FullFuncts [Funcs]
  deriving Show

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
preproc ('/':xs)                         = " / " ++ preproc xs 
preproc ('=':'=':xs)                     = " == " ++ preproc xs
preproc ('=':xs)                         = " = " ++ preproc xs
preproc ('.':xs)                         = " . " ++ preproc xs
preproc ('<':xs)                         = " < " ++ preproc xs
preproc (x:xs)                           = x : preproc xs

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
--class stuff will be known as keywords(look at classifyT for ex)--
classify s@("public")  = Keyword s
classify s@("private") = Keyword "public"--for simplicity
classify s@("class")   = Keyword s
classify s@("main")    = Keyword s
classify s@("new")     = Keyword s
classify s@("static")  = Keyword s
classify s@("args")    = Keyword s
classify s@("this")    = Keyword s
classify s@("return")  = Keyword s
classify s@("System")  = Keyword s
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
classify "void"    = Type Vid
---------- Operators -------------
classify "-"         = Op SubOp
classify "+"         = Op AddOp
classify "*"         = Op MulOp
classify "\\"        = Op DivOp
classify "=="        = Op EqlOp
classify "="         = Op AssignOp
classify "."         = Op DotOp
classify "<"         = Op LesserOp
classify s | isCSym s = Csym (read s)
classify (x:xs) = VSym (x:xs)

isCSym :: String -> Bool
isCSym "" = False
isCSym (x:xs) = isDigit x && q1 xs
  where q1 "" = True
        q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
        q2 ys = all isDigit ys

lexer :: String -> [Token]
lexer x = map classify (words (preproc x))

------------------- SR0 Function --------------------------------

sr0 :: [Token] -> [Token] -> [Token]
sr0 (VSym x:Type y:stack) input                                                    = sr0 (Declaration (y,x):stack) input
sr0 (RPar:Declaration x:stack) input                                               = sr0 (FuncPar [x]:stack) input
sr0 (Semi:Declaration (t, name):Keyword "public":stack) input                      = sr0 (VarDec (t, name):stack) input
sr0 (Keyword "print":Op DotOp:Keyword "out":Op DotOp:Keyword "System":stack) input = sr0 (TPrint:stack) input
sr0 (RPar:LPar:stack) input                                                        = sr0 (FuncPar []:stack) input
sr0 (FuncPar x:LPar:stack) input                                                   = sr0 (FuncPar x:stack) input
sr0 (LBra:FuncPar x:stack) input                                                   = sr0 (FuncPar x :stack) input
sr0 (FuncPar x:Com:Declaration y:stack) input                                      = sr0 (FuncPar (y:x) :stack) input
sr0 (FuncPar x:Declaration (y, z):Keyword "public":stack) input                    = sr0 (AlmostFunc (y, z, x) :stack) input
sr0 (RBra:StateList list:LBra:CFuncHeader (x, params):stack) input                 = sr0 (FullCFunct (x, params, list):stack) input
sr0 (RBra:StateList list:LBra:AlmostFunc (ret, name, param):stack) input           = sr0 (FullFunct (ret,name,param,list):stack) input
sr0 (RPar:Keyword "args":Brac:Type Str: LPar:Keyword "main":Type Vid: Keyword "static":Keyword "public":LBra:Keyword "main":Keyword"class":Keyword "public":stack) input 
                                                                                   = sr0 (MainM:stack) input
sr0 (FuncPar pars:VSym name:Op DotOp:VSym obj:stack) input                         = sr0 (ObJCall (obj, name, pars):stack) input
sr0 (RPar:ObJCall x:LPar:TPrint:stack) input                                       = sr0 (APrint (ObJCall x):stack) input
sr0 (Semi:VSym expr:Op AssignOp:VSym var:stack) input                              = sr0 (States (ReAssign var expr):stack) input
sr0 (Qut:VSym x:Qut:stack) input                                                   = sr0 (VSym ("\"" ++ x ++ "\""):stack) input
sr0 (Semi:RPar:VSym x:stack) input                                                 = sr0 (ConPar [x]:stack) input
sr0 (Semi:RPar:Csym x:stack) input                                                 = sr0 (ConPar [show x]:stack) input
sr0 (ConPar x:LBra:stack) input                                                    = sr0 (ConPar x :stack) input
sr0 (ConPar x:Com:Csym y:stack) input                                              = sr0 (ConPar (show y:x):stack) input
sr0 (ConPar x:Com:VSym y:stack) input                                              = sr0 (ConPar (y:x) :stack) input
sr0 (LBra:VSym x:Keyword "class":stack) input                                      = sr0 (ClassName x:stack) input
sr0 (FuncPar x:VSym y:Keyword "public":stack) input                                = sr0 (CFuncHeader (y, x):stack) input
sr0 (Semi:VSym x:Keyword "return":stack) input                                     = sr0 (States (Ret (Var x)):stack) input
sr0 (Op DotOp:Keyword "this":stack) input                                          = sr0 (ClassCall:stack) input
sr0 (StateList (((ReAssign y x):xs)):ClassCall:stack) input                        = sr0 (States (TReAssign y x):stack) input
sr0 (ConPar param:LPar:VSym _:Keyword "new":Op AssignOp:VSym y:VSym x:stack) input = sr0 (States (OInst (x, y, param)):stack) input
sr0 (ObJCall x:Op y:ObJCall z:stack) input                                         = sr0 (PA (Sub (AeObjCall z) (AeObjCall x)):stack) input
sr0 (Semi:APrint (ObJCall x):stack) input                                          = sr0 (States (ObjectPrint x):stack) input 
sr0 (RPar:PA x:LPar:VSym y:Op DotOp:VSym z:stack) input                            = sr0 (ObjCallAe (z, y, x):stack) input
sr0 (VarDec y:VarList x:stack) input                                               = sr0 (VarList (y:x):stack) input
sr0 (VarDec x:stack) input                                                         = sr0 (VarList [x]:stack) input
sr0 (Semi:ObjCallAe (x,y,z):stack) input                                           = sr0 (States (ObjFCall (x,y,z)):stack) input
sr0 (States y:StateList x:stack) input                                             = sr0 (StateList (x ++ [y]):stack) input
sr0 (States x:stack) input                                                         = sr0 (StateList [x]:stack) input
sr0 (FullFunct y:FullFuncts x:stack) input                                         = sr0 (FullFuncts (x ++ [y]):stack) input
sr0 (FullFunct x:stack) input                                                      = sr0 (FullFuncts [x]:stack) input
sr0 stack (i:input)                                                                = sr0 (i:stack) input
sr0 stack []                                                                       = reverse stack

finalConv :: [Token] -> [Decs]
finalConv (RBra:xs) = finalConv xs
finalConv [] = []
finalConv (ClassName x:VarList var:FullCFunct con:FullFuncts functs:xs) = CDec (x, FullCFunct con,var,functs) : finalConv (VSym x:xs)
finalConv (VSym name:MainM:LBra:StateList x:xs) = DDec (name, x) : finalConv xs

------ Test Zero ---------
-- [ClassName "Student",VarList [(Int,"age"),(Str,"name")],FullCFunct ("Student",[(Str,"n"),(Int,"a")],[ReAssign "age" "a",ReAssign "name" "n"])
--  ,FullFuncts [(Vid,"setAge",[(Int,"a")],[TReAssign "age" "a"]),(Int,"getAge",[],[Ret (Var "age")]),(Vid,"setName",[(Str,"
--  n")],[TReAssign "name" "n"]),(Str,"getName",[],[Ret (Var "name")])],RBra,MainM,LBra,StateList [ObjFCall ("Jacob","setAge",Sub 
--  (AeObjCall ("Wolfe","getAge",[])) (AeObjCall ("Luke","getAge",[]))),OInst ("Student","Luke",["\"Luke\"","2"]),OInst ("Student","Wolfe",
--  ["\"Wolfe\"","999"]),OInst ("Student","Jacob",["\"Jacob\"","100"])],APrint (ObJCall ("Jacob","getAge",[])),Semi,RBra,RBra]

-- [CDec ("Student",FullCFunct ("Student",[(Str,"n"),(Int,"a")],[ReAssign "name" "n",ReAssign "age" "a"]),[(Int,"age"),(Str,"name")],
-- [(Str,"getName",[],[Ret (Var "name")]),(Vid,"setName",[(Str,"n")],[TReAssign "name" "n"]),(Int,"getAge",[],[Ret (Var "age")]),
-- (Vid,"setAge",[(Int,"a")],[TReAssign "age" "a"])]) DDec [OInst ("Student","Jacob",["\"Jacob\"","100"]),OInst ("Student","Wolfe",
-- ["\"Wolfe\"","999"]),OInst ("Student","Luke",["\"Luke\"","2"]),ObjFCall ("Jacob","setAge",Sub (AeObjCall ("Wolfe","getAge",[])) 
-- (AeObjCall ("Luke","getAge",[]))),ObjectPrint ("Jacob","getAge",[])]]



--testParse = [Keyword "class",VSym "Student",LBra,Keyword "public",Type Str,VSym "name",Semi,Keyword "public",Type Int,VSym "age",Semi,Keyword "public",VSym "Student",LPar,Type Str,VSym "n",Com,Type Int,VSym "a",RPar,LBra,VSym "name",Op AssignOp,VSym "n",Semi,VSym "age",Op AssignOp,VSym "a",Semi,RBra,Keyword "public",Type Str,VSym "getName",LPar,RPar,LBra,Keyword "return",VSym "name",Semi,RBra,Keyword "public",Type Vid,VSym "setName",LPar,Type Str,VSym "n",RPar,LBra,Keyword "this",Op DotOp,VSym "name",Op AssignOp,VSym "n",Semi,Bra,Keyword "public",Type Int,VSym "getAge",LPar,RPar,LBra,Keyword "return",VSym "age",Semi,RBra,Keyword "public",Type Vid,VSym "setAge",LPar,Type Int,VSym "a",RPar,LBra,Keyword "this",Op DotOp,VSym "age",Op AssignOp,VSym "a",Semi,RBra,RBra,Keyword "public",Keyword "class",Keyword "main",LBra,Keyword "public",Keyword "static",Type Vid,Keyword "main",LPar,Type Str,Brac,Keyword "args",RPar,LBra,VSym "Student",VSym "Jacob",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Jacob",Qut,Com,Csym 100,RPar,Semi,VSym "Student",VSym "Wolfe",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Wolfe",Qut,Com,Csym 999,RPar,Semi,VSym "Student",VSym "Luke",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Luke",Qut,Com,Csym 2,RPar,Semi,VSym "Jacob",Op DotOp,VSym "setAge",LPar,VSym "Wolfe",Op DotOp,VSym "getAge",LPar,RPar,Op SubOp,VSym "Luke",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,Keyword "System",Op DotOp,Keyword "out",Op DotOp,Keyword "print",LPar,VSym "Jacob",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,RBra,RBra]
testParse = [Keyword "class",VSym "Student",LBra,Keyword "public",Type Str,VSym "name",Semi,Keyword "public",Type Int,VSym "age",Semi,Keyword "public",VSym "Student",LPar,Type Str,VSym "n",Com,Type Int,VSym "a",RPar,LBra,VSym "name",Op AssignOp,VSym "n",Semi,VSym "age",Op 
 AssignOp,VSym "a",Semi,RBra,Keyword "public",Type Str,VSym "getName",LPar,RPar,LBra,Keyword "return",VSym "name",Semi,RBra,Keyword "public",Type Vid,VSym "setName",LPar,Type Str,VSym "n",RPar,LBra,Keyword "this",Op DotOp,VSym "name",Op AssignOp,VSym "n",Semi,RBra,Keyword "public",Type Int,VSym "getAge",LPar,RPar,LBra,Keyword "return",VSym "age",Semi,RBra,Keyword "public",Type Vid,VSym "setAge",LPar,Type Int,VSym "a",RPar,LBra,Keyword "this",Op DotOp,VSym "age",Op AssignOp,VSym "a",Semi,RBra,RBra,Keyword "public",Keyword "class",Keyword "main",LBra,Keyword "public",Keyword "static",Type Vid,Keyword "main",LPar,Type Str,Brac,Keyword "args",RPar,LBra,VSym "Student",VSym "Jacob",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Jacob",Qut,Com,Csym 100,RPar,Semi,VSym "Student",VSym "Wolfe",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Wolfe",Qut,Com,Csym 999,RPar,Semi,VSym "Student",VSym "Luke",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Luke",Qut,Com,Csym 2,RPar,Semi,VSym "Jacob",Op DotOp,VSym "setAge",LPar,VSym "Wolfe",Op DotOp,VSym "getAge",LPar,RPar,Op SubOp,VSym "Luke",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,Keyword "System",Op DotOp,Keyword "out",Op DotOp,Keyword "print",LPar,VSym "Jacob",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,RBra,RBra]
test1 = sr0 [] testParse
test2 = finalConv test1
------------------------------------------------------------------------------------

cDriver :: Decs -> String
cDriver (DDec (n, x)) = "int main() {" ++ statementLister n x ++ "}"
cDriver (CDec (w, x, y, z)) = "struct " ++ w ++ " {" ++ fieldLister y ++ "}; " 
 ++ constructorBuilder w x ++ statementListerFancy w z 


constructorBuilder :: String -> Token -> String
constructorBuilder n (FullCFunct (x,y,z)) = "struct " ++ n ++ " " ++ n ++ "_" ++ x ++ "(" ++ paramasPLister y ++ ") { " ++ 
                                            "struct " ++ n ++ " new" ++ n ++ "; " ++ funcyBuilder n z

funcyBuilder :: String -> [Statements] -> String
funcyBuilder _ [] = []
funcyBuilder name ((ReAssign var functs):xs) = name ++ "." ++ var ++ "=" ++ functs ++ "; " ++ funcyBuilder name xs
funcyBuilder name (Ret (Var x):xs) = "return " ++ x ++ ";" ++ funcyBuilder name xs


statementListerFancy :: String -> [Funcs] -> String
statementListerFancy _ [] = ""
statementListerFancy name ((w, x, params, list):xs) | dataConv  w /= "void " = dataConv w ++ " " ++ name ++ "_" ++ x ++ 
  "(struct " ++ name ++ "s" ++ paramBuilder params ++ "}" ++ statementListerFancy name xs
                                                    | otherwise = dataConv w ++ " " ++ name ++ "_" ++ x ++ 
  "(struct " ++ name ++ "*s" ++ paramBuilder params ++ "}" ++ statementListerFancy name xs

paramBuilder :: [Params] -> String
paramBuilder [] = ")"
paramBuilder ((x,y):xs) = "," ++ dataConv x ++ " " ++ y

dataConv :: Types -> String
dataConv Str = "char* " 
dataConv Int = "int " 
dataConv Dble = "double "
dataConv Chr = "char " 
dataConv Shrt = "short "
dataConv Bllean = "int "
dataConv Bte = "byte " 
dataConv Flt = "float "
dataConv Vid = "void "
dataConv _ = " " 


fieldLister :: [Params] -> String
fieldLister [] = ""
fieldLister ((x,y):xs) = dataConv x ++ y ++ "; " ++ fieldLister xs


paramasPLister :: [Params] -> String
paramasPLister [] = ""
paramasPLister ((x,y):xs) = dataConv x ++ " " ++ y ++ ","

argsLister :: [Arguments] -> String
argsLister [x] = x
argsLister (x:xs) = x ++ "," ++ argsLister xs

aexprPrinter :: String -> AExpr -> String
aexprPrinter n (Var x) = show x
aexprPrinter n (Const x) = show x
aexprPrinter n (Add x y) = aexprPrinter n x ++ "+" ++ aexprPrinter n y
aexprPrinter n (Sub x y) = aexprPrinter n x ++ "-" ++ aexprPrinter n y
aexprPrinter n (Mul x y) = aexprPrinter n x ++ "*" ++ aexprPrinter n y
aexprPrinter n (Div x y) = aexprPrinter n x ++ "/" ++ aexprPrinter n y
aexprPrinter n (AeObjCall (x,y,z)) = n ++ "_" ++ y ++ "(" ++ paramasPLister z ++ ")"

statementLister :: String -> [Statements] -> String
statementLister _ [] = []
statementLister name ((ReAssign x z):xs) = x ++ " " ++ " = " ++ " " ++ z ++ "; " ++ statementLister name xs
statementLister name ((TReAssign nam exp):xs) = "s->" ++ nam ++ " = " ++ exp ++ ";" ++ statementLister name xs
statementLister name ((ObjectPrint (obj, funtn, params)):xs) = "printf(\"%d\\n\"," ++ name ++ "_" ++ obj ++ "(" ++ paramasPLister params ++ ");" ++ statementLister name xs
statementLister name ((Ret (Var x)):xs) = "return " ++ x ++ ";"
statementLister name ((OInst (str, nam, arg)):xs) = "struct " ++ name ++ " " ++ nam ++ " = " ++ name ++ "_" ++ name ++ "(" ++ argsLister arg ++ ")"
statementLister name ((ObjFCall (tpe, nam, expr)):xs) = name ++ "_" ++ nam ++ "(" ++ "&" ++ tpe ++ aexprPrinter name expr ++ ");"
