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
--type Funcs = (Types, FName, [Params], [Statements])

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

-- classifyT :: [Token] -> [Token]
-- classifyT [] = []
-- classifyT (Keyword "public":Keyword "class":xs) = Class : classifyT xs
-- classifyT (Keyword "public":Keyword "static":Keyword "void":Keyword "main"
--            :LPar:(Type Str):Brac:Keyword "args":xs) = MainM : classifyT xs
-- classifyT (x:xs)                                      = x : classifyT xs

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
preproc ('/':xs)                         = " / " ++ preproc xs 
preproc ('=':'=':xs)                     = " == " ++ preproc xs
preproc ('=':xs)                         = " = " ++ preproc xs
preproc ('.':xs)                         = " . " ++ preproc xs
preproc ('<':xs)                         = " < " ++ preproc xs
preproc (x:xs)                           = x : preproc xs

lexer :: String -> [Token]
lexer x = map classify (words (preproc x))

-- data Token = Class | MainM | VSym String | Csym Integer | LPar | RPar | LBra |
--              RBra | Semi | Com | Coln | Qut | Brac | Keyword String | Type Types |
--              Op Ops | Err | PA AExpr | PB BExpr | Declaration CVars | Assignment AExpr | ReAssignment Statements|
--              States Statements 
--   deriving Show

-- sr (RBra : PI i : stack) input                      = sr (PI (Do [i]) : stack) input
-- sr (RBra : stack) input                             = sr (PI (Do []) : stack) input
-- sr (PI (Do i) : Semi : PI x : stack) input          = sr (PI (Do (x:i)) : stack) input
-- sr (PI (Do i) : LBra : stack) input                 = sr (PI (Do i) : stack) input
--Student Jacob = new Student("Jacob",100);

------------------- SR0 Function --------------------------------

sr0 :: [Token] -> [Token] -> [Token]
sr0 (VSym x:Type y:stack) input = sr0 (Declaration (y,x):stack) input
--Keyword "public",VSym "Student",LPar,FuncPar [(Str,"n"),(Int,"a")]
--CFunctHeader ("Student", FuncPar [(Str,"n"),(Int,"a")])

--Keyword "public",Declaration (Str,"getName"),FuncPar []
--Keyword "public",VSym "Student",FuncPar [(Str,"n"),(Int,"a")]
--FunctHeader :: AlmostFunc
--FunctHeader ("getName",Str, [])
sr0 (RPar:Declaration x:stack) input = sr0 (FuncPar [x]:stack) input

--Keyword "public",Declaration (Str,"name"),Semi
--VarDec (Str, "name")
sr0 (Semi:Declaration (t, name):Keyword "public":stack) input = sr0 (VarDec (t, name):stack) input
sr0 (Keyword "print":Op DotOp:Keyword "out":Op DotOp:Keyword "System":stack) input 
 = sr0 (TPrint:stack) input

sr0 (RPar:LPar:stack) input = sr0 (FuncPar []:stack) input
sr0 (FuncPar x:LPar:stack) input = sr0 (FuncPar x:stack) input
sr0 (LBra:FuncPar x:stack) input = sr0 (FuncPar x :stack) input
sr0 (FuncPar x:Com:Declaration y:stack) input = sr0 (FuncPar (y:x) :stack) input
sr0 (FuncPar x:Declaration (y, z):Keyword "public":stack) input = sr0 (AlmostFunc (y, z, x) :stack) input

--CFuncHeader ("Student",[(Str,"n"),(Int,"a")]),LBra,StateList [ReAssign "age" "a",ReAssign "name" "n"],RBra
sr0 (RBra:StateList list:LBra:CFuncHeader (x, params):stack) input = sr0 (FullCFunct (x, params, list):stack) input

--AlmostFunc (Str,"getName",[]),LBra,StateList [Ret (Var "name")],RBra
sr0 (RBra:StateList list:LBra:AlmostFunc (ret, name, param):stack) input = sr0 (FullFunct (ret,name,param,list):stack) input

--Keyword "public",Keyword "class",Keyword "main",LBra,Key
-- word "public",Keyword "static",Type Vid,Keyword "main",LPar,Type Str,Brac,Keyword "args",RPar

sr0 (RPar:Keyword "args":Brac:Type Str: LPar:Keyword "main":Type Vid: Keyword "static":Keyword "public":   -- JACOB'S BIG BOY LINE!!!!!! (Main)
     LBra:Keyword "main":Keyword"class":Keyword "public":stack) input = sr0 (MainM:stack) input

--System.out.print(Jacob.getAge());
--Keyword "System",Op DotOp,Keyword "out",Op DotOp,Keyword "print",LPar
--VSym "Jacob",Op DotOp,VSym "setAge",LPar,

sr0 (FuncPar pars:VSym name:Op DotOp:VSym obj:stack) input
 = sr0 (ObJCall (obj, name, pars):stack) input

--TPrint,LPar,ObJCall ("Jacob","getAge",[]),RPar
sr0 (RPar:ObJCall x:LPar:TPrint:stack) input = sr0 (APrint (ObJCall x):stack) input
-- sr0 (RPar:VSym x:LPar:TPrint:stack) input = sr0 (APrint :stack) input
-- sr0 (RPar:Csym x:LPar:TPrint:stack) input = sr0 (APrint :stack) input
-- sr0 (RPar:x:LPar:TPrint:stack) input = sr0 (APrint :stack) input


--VSym "name",Op AssignOp,VSym "n",Semi
--VSym "age",Op AssignOp,VSym "a",Semi
sr0 (Semi:VSym expr:Op AssignOp:VSym var:stack) input = sr0 (States (ReAssign var expr):stack) input

sr0 (Qut:VSym x:Qut:stack) input = sr0 (VSym ("\"" ++ x ++ "\""):stack) input
sr0 (Semi:RPar:VSym x:stack) input = sr0 (ConPar [x]:stack) input
sr0 (Semi:RPar:Csym x:stack) input = sr0 (ConPar [show x]:stack) input
sr0 (ConPar x:LBra:stack) input = sr0 (ConPar x :stack) input
sr0 (ConPar x:Com:Csym y:stack) input = sr0 (ConPar (show y:x):stack) input
sr0 (ConPar x:Com:VSym y:stack) input = sr0 (ConPar (y:x) :stack) input



--Keyword "class",VSym "Student"
sr0 (LBra:VSym x:Keyword "class":stack) input = sr0 (ClassName x:stack) input

--Keyword "public",VSym "Student",FuncPar [(Str,"n"),(Int,"a")]
sr0 (FuncPar x:VSym y:Keyword "public":stack) input = sr0 (CFuncHeader (y, x):stack) input

--VSym "System",Op DotOp,Keyword "out",Op DotOp,Keyword "print",LPar,VSym "Jacob",Op DotOp,VSym "getAge",FuncPar [],RPar,Semi

sr0 (Semi:VSym x:Keyword "return":stack) input = sr0 (States (Ret (Var x)):stack) input
sr0 (Op DotOp:Keyword "this":stack) input = sr0 (ClassCall:stack) input
--Keyword "this",Op DotOp,VSym "age",Op AssignOp,VSym "a",Semi
sr0 (StateList (((ReAssign y x):xs)):ClassCall:stack) input = sr0 (States (TReAssign y x):stack) input
sr0 (ConPar param:LPar:VSym _:Keyword "new":Op AssignOp:VSym y:VSym x:stack) input = sr0 (States (OInst (x, y, param)):stack) input

--ObJCall ("Wolfe","getAge",[]),Op SubOp,ObJCall ("Luke","getAge",[])
sr0 (ObJCall x:Op y:ObJCall z:stack) input = sr0 (PA (Sub (AeObjCall z) (AeObjCall x)):stack) input

--print stuff to staement print stuff idk
sr0 (Semi:APrint (ObJCall x):stack) input = sr0 (States (ObjectPrint x):stack) input 

--VSym "Jacob",Op DotOp,VSym "setAge",LPar,PA (Sub (AeObjCall ("Wolfe","getAge",[])) (AeObjCall ("Luke","getAge",[])))
sr0 (RPar:PA x:LPar:VSym y:Op DotOp:VSym z:stack) input = sr0 (ObjCallAe (z, y, x):stack) input

sr0 (VarDec y:VarList x:stack) input = sr0 (VarList (y:x):stack) input
sr0 (VarDec x:stack) input = sr0 (VarList [x]:stack) input

--MainM,LBra,StateList [OInst ("Student","Luke",["\"Luke\"","2"]),OInst ("Student","Wolfe",["\"Wolfe\"","999"]),OInst ("Student","Jacob",["\"Jacob\"","100"])],
--ObjCallAe ("Jacob","setAge",Sub (AeObjCall ("Wolfe","getAge",[])) (AeObjCall ("Luke","getAge",[]))),Semi,APrint (ObJCall ("Jacob","getAge",[])),Semi,
sr0 (Semi:ObjCallAe (x,y,z):stack) input = sr0 (States (ObjFCall (x,y,z)):stack) input

sr0 (States y:StateList x:stack) input = sr0 (StateList (x ++ [y]):stack) input
sr0 (States x:stack) input = sr0 (StateList [x]:stack) input
--sr0 (:stack) input = sr0 (:stack) input
sr0 (FullFunct y:FullFuncts x:stack) input = sr0 (FullFuncts (x ++ [y]):stack) input
sr0 (FullFunct x:stack) input = sr0 (FullFuncts [x]:stack) input
sr0 stack (i:input) = sr0 (i:stack) input
sr0 stack [] = reverse stack

--sr (:stack) input = sr (:stack) input
-- sr :: [Token] -> [Token] -> [Token]
--sr (:stack) input = sr (:stack) input
-- sr (RBra:StateList x:LBra:MainM:stack) input = sr (DDec ("main"):stack) input
-- sr stack (i:input) = sr0 (i:stack) input
-- sr stack [] = reverse stack
finalConv :: [Token] -> [Decs]
finalConv (RBra:xs) = finalConv xs
finalConv [] = []
finalConv (ClassName x:VarList var:FullCFunct con:FullFuncts functs:xs) = CDec (x, FullCFunct con,var,functs) : finalConv (VSym x:xs)
finalConv (VSym name:MainM:LBra:StateList x:xs) = DDec (name, x) : finalConv xs

------ Test Zero ---------
-- [ClassName "Student",VarList [(Int,"age"),(Str,"name")],FullCFunct ("Student",[(Str,"n"),(Int,"a")],[ReAssign "name" "n",ReAssign "age" "a"])
--  ,FullFuncts [(Str,"getName",[],[Ret (Var "name")]),(Vid,"setName",[(Str,"n")],[TReAssign "name" "n"]),(Int,"getAge",[],[ Ret (Var "age")]),
--  (Vid,"setAge",[(Int,"a")],[TReAssign "age" "a"])],RBra,MainM,LBra,StateList [OInst ("Student","Jacob",["\"Jacob\"","100"]),
--  OInst ("Student","Wolfe",["\"Wolfe\"","999"]),OInst ("Student","Luke",["\"Luke\"","2"]),ObjFCall ("Jacob","setAge",Sub (
--  AeObjCall ("Wolfe","getAge",[])) (AeObjCall ("Luke","getAge",[]))),ObjectPrint ("Jacob","getAge",[])],RBra,RBra]


-- [ClassName "Student",VarList [(Int,"age"),(Str,"name")],FullCFunct ("Student",[(Str,"n"),(Int,"a")],[ReAssign "age" "a",ReAssign "name" "n"])
--  ,FullFuncts [(Vid,"setAge",[(Int,"a")],[TReAssign "age" "a"]),(Int,"getAge",[],[Ret (Var "age")]),(Vid,"setName",[(Str,"
--  n")],[TReAssign "name" "n"]),(Str,"getName",[],[Ret (Var "name")])],RBra,MainM,LBra,StateList [ObjFCall ("Jacob","setAge",Sub 
--  (AeObjCall ("Wolfe","getAge",[])) (AeObjCall ("Luke","getAge",[]))),OInst ("Student","Luke",["\"Luke\"","2"]),OInst ("Student","Wolfe",
--  ["\"Wolfe\"","999"]),OInst ("Student","Jacob",["\"Jacob\"","100"])],APrint (ObJCall ("Jacob","getAge",[])),Semi,RBra,RBra]

-- [CDec ("Student",FullCFunct ("Student",[(Str,"n"),(Int,"a")],[ReAssign "name" "n",ReAssign "age" "a"]),[(Int,"age"),(Str,"name")],[(Str,"getName",[],[Ret (Var "name")]),(Vid,"setName",[(Str,"n")],[TReAssign "name" "n"]),(Int,"getAge",[],[Ret (Var "age")]),(Vid,"setAge",[(Int,"a")],[TReAssign "age" "a"])])

--  DDec [OInst ("Student","Jacob",["\"Jacob\"","100"]),OInst ("Student","Wolfe",["\"Wolfe\"","999"]),
--   OInst ("Student","Luke",["\"Luke\"","2"]),ObjFCall ("Jacob","setAge",Sub (AeObjCall ("Wolfe","getAge",[])) (AeObjCall
--  ("Luke","getAge",[]))),ObjectPrint ("Jacob","getAge",[])]]



--testParse = [Keyword "class",VSym "Student",LBra,Keyword "public",Type Str,VSym "name",Semi,Keyword "public",Type Int,VSym "age",Semi,Keyword "public",VSym "Student",LPar,Type Str,VSym "n",Com,Type Int,VSym "a",RPar,LBra,VSym "name",Op AssignOp,VSym "n",Semi,VSym "age",Op AssignOp,VSym "a",Semi,RBra,Keyword "public",Type Str,VSym "getName",LPar,RPar,LBra,Keyword "return",VSym "name",Semi,RBra,Keyword "public",Type Vid,VSym "setName",LPar,Type Str,VSym "n",RPar,LBra,Keyword "this",Op DotOp,VSym "name",Op AssignOp,VSym "n",Semi,Bra,Keyword "public",Type Int,VSym "getAge",LPar,RPar,LBra,Keyword "return",VSym "age",Semi,RBra,Keyword "public",Type Vid,VSym "setAge",LPar,Type Int,VSym "a",RPar,LBra,Keyword "this",Op DotOp,VSym "age",Op AssignOp,VSym "a",Semi,RBra,RBra,Keyword "public",Keyword "class",Keyword "main",LBra,Keyword "public",Keyword "static",Type Vid,Keyword "main",LPar,Type Str,Brac,Keyword "args",RPar,LBra,VSym "Student",VSym "Jacob",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Jacob",Qut,Com,Csym 100,RPar,Semi,VSym "Student",VSym "Wolfe",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Wolfe",Qut,Com,Csym 999,RPar,Semi,VSym "Student",VSym "Luke",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Luke",Qut,Com,Csym 2,RPar,Semi,VSym "Jacob",Op DotOp,VSym "setAge",LPar,VSym "Wolfe",Op DotOp,VSym "getAge",LPar,RPar,Op SubOp,VSym "Luke",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,Keyword "System",Op DotOp,Keyword "out",Op DotOp,Keyword "print",LPar,VSym "Jacob",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,RBra,RBra]
testParse = [Keyword "class",VSym "Student",LBra,Keyword "public",Type Str,VSym "name",Semi,Keyword "public",Type Int,VSym "age",Semi,Keyword "public",VSym "Student",LPar,Type Str,VSym "n",Com,Type Int,VSym "a",RPar,LBra,VSym "name",Op AssignOp,VSym "n",Semi,VSym "age",Op 
 AssignOp,VSym "a",Semi,RBra,Keyword "public",Type Str,VSym "getName",LPar,RPar,LBra,Keyword "return",VSym "name",Semi,RBra,Keyword "public",Type Vid,VSym "setName",LPar,Type Str,VSym "n",RPar,LBra,Keyword "this",Op DotOp,VSym "name",Op AssignOp,VSym "n",Semi,RBra,Keyword "public",Type Int,VSym "getAge",LPar,RPar,LBra,Keyword "return",VSym "age",Semi,RBra,Keyword "public",Type Vid,VSym "setAge",LPar,Type Int,VSym "a",RPar,LBra,Keyword "this",Op DotOp,VSym "age",Op AssignOp,VSym "a",Semi,RBra,RBra,Keyword "public",Keyword "class",Keyword "main",LBra,Keyword "public",Keyword "static",Type Vid,Keyword "main",LPar,Type Str,Brac,Keyword "args",RPar,LBra,VSym "Student",VSym "Jacob",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Jacob",Qut,Com,Csym 100,RPar,Semi,VSym "Student",VSym "Wolfe",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Wolfe",Qut,Com,Csym 999,RPar,Semi,VSym "Student",VSym "Luke",Op AssignOp,Keyword "new",VSym "Student",LPar,Qut,VSym "Luke",Qut,Com,Csym 2,RPar,Semi,VSym "Jacob",Op DotOp,VSym "setAge",LPar,VSym "Wolfe",Op DotOp,VSym "getAge",LPar,RPar,Op SubOp,VSym "Luke",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,Keyword "System",Op DotOp,Keyword "out",Op DotOp,Keyword "print",LPar,VSym "Jacob",Op DotOp,VSym "getAge",LPar,RPar,RPar,Semi,RBra,RBra]
test1 = sr0 [] testParse
test2 = finalConv test1
-- parser :: [Token] -> [DDec]
-- parser input = case sr [] input of
--          [PI s] -> s
--          l      -> error ("bad parse" ++ show l)
------------------------------------------------------------------------------------



cDriver :: Decs -> String
cDriver (DDec (n, x)) = "int main() {" ++ statementLister n x ++ "}"
cDriver (CDec (w, x, y, z)) = "struct " ++ w ++ " {" ++ paramsLister y ++ "}; " 
 ++ constructerBuilder w x ++ statementListerFancy w z 


constructerBuilder :: String -> Token -> String
constructerBuilder n (FullCFunct (x,y,z)) = "struct " ++ n ++ " " ++ n ++ "_" ++ x ++ "(" ++ paramasPLister y ++ ") {" ++ statementLister n z

funcyBuilder :: String -> Statements -> String
funcyBuilder name (ReAssign var functs) = name ++ "." ++ var ++ "=" ++ functs

statementListerFancy :: String -> [Funcs] -> String
statementListerFancy _ [] = ""
statementListerFancy name ((w, x, params, list):xs) | dataConv  w /= "void " = dataConv w ++ " " ++ name ++ "_" ++ x ++ 
  "(struct " ++ name ++ "s" ++ paramBuilder params ++ "}" ++ statementListerFancy name xs
                                                    | otherwise = dataConv w ++ " " ++ name ++ "_" ++ x ++ 
  "(struct " ++ name ++ "*s" ++ paramBuilder params ++ "}" ++ statementListerFancy name xs
--type Params = (Types, PVar)
paramBuilder :: [Params] -> String
paramBuilder [] = ")"
paramBuilder ((x,y):xs) = "," ++ dataConv x ++ " " ++ y

dataConv :: Types -> String
dataConv Str = "char* " 
dataConv Int = "int " 
dataConv Dble = "double "
dataConv Chr = "char " 
dataConv Shrt = "short "
dataConv Bllean = "boolean "
dataConv Bte = "byte " 
dataConv Flt = "float "
dataConv Vid = "void "
dataConv _ = " " 


paramsLister :: [Params] -> String
paramsLister [] = ""
paramsLister ((x,y):xs) = dataConv x ++ y ++ "; " ++ paramsLister xs


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
--aexprPrinter n (FCall (w,x,y,z) v) = n ++ "_" ++ x ++ "(" ++ paramasPLister y + ")"
aexprPrinter n (AeObjCall (x,y,z)) = n ++ "_" ++ y ++ "(" ++ paramasPLister z ++ ")"

statementLister :: String -> [Statements] -> String
statementLister _ [] = []
statementLister name ((ReAssign x z):xs) = x ++ " " ++ " = " ++ " " ++ z ++ "; " ++ statementLister name xs
statementLister name ((TReAssign nam exp):xs) = "s->" ++ nam ++ " = " ++ exp ++ ";" ++ statementLister name xs
--,ObjectPrint ("Jacob","getAge",[])]]
statementLister name ((ObjectPrint (obj, funtn, params)):xs) = "printf(\"%d\\n\"," ++ name ++ "_" ++ obj ++ "(" ++ paramasPLister params ++ ");" ++ statementLister name xs
statementLister name ((Ret (Var x)):xs) = "return " ++ x ++ ";"
--OInst ("Student","Jacob",["\"Jacob\"","100"]
statementLister name ((OInst (str, nam, arg)):xs) = "struct " ++ name ++ " " ++ nam ++ " = " ++ name ++ "_" ++ name ++ "(" ++ argsLister arg ++ ")"
--ObjFCall ("Jacob","setAge",Sub (AeObjCall ("Wolfe","getAge",[])) (AeObjCall
--  ("Luke","getAge",[]))
statementLister name ((ObjFCall (tpe, nam, expr)):xs) = name ++ "_" ++ nam ++ "(" ++ "&" ++ tpe ++ aexprPrinter name expr ++ ");"


-- main :: IO ()
-- main = do
--   putStrLn "Enter name of file:"
--   filename <- getLine 
--   contents <- readFile filename
--   let analyzed = lexer contents
--   putStrLn "Here is the result of lexical analysis:"
--   putStrLn (show analyzed)
--   putStrLn "------------------------------------------"
--   let parsed = parseInstrs analyzed
--   putStrLn "Here is the result of parsing:"
--   putStrLn (show parsed)
--   putStrLn "------------------------------------------"
--   let executed = run (reverse parsed)
--   putStrLn "Here is the result of execution:"
--   putStrLn (show executed)

  

{- When making [AExpr] we need to store arguments like "&objectReference", *****************this may not work ): 
   example: student_setAge(&Jacob,student_getAge(Wolfe) - student_getAge(Luke)) would look like 
   [Var "&Jacob", Sub (FCall (int, student_getAge,_,_) ["Wolfe"]) (FCall (int, student_getAge,_,_) ["Luke"])]-}
--statementLister [FCall2 x y] = callBuilder x y
--statementLister ((FCall2 x y):xs) = callBuilder x y ++ statementLister xs

-- -- Check if data type of variable needs to be changed from Java formatting to C
-- dataConv :: String -> String
-- dataConv x | x == "String" = "char*"
--            | otherwise = x 

-- -- Check if variable is a string and needs quotation marks
-- strCheck :: String -> String -> String
-- strCheck x y | x == "String" = "\"" ++ y ++ "\""
--              | otherwise = y

-- -- List AExpr's in arguments with comma separator
-- aExListToC :: [AExpr] -> String
-- aExListToC [x] = aExToC x
-- aExListToC (x:xs) = aExToC x ++ "," ++ aExListToC xs
-- toParen :: AExpr -> Bool
-- toParen (Var x) = False
-- toParen (Const x) = False
-- toParen _ = True;
-- putParen :: AExpr -> String
-- putParen z = if toParen z then "(" ++ aExToC z ++ ")" else aExToC z
-- -- For converting individual AExpr's
-- aExToC :: AExpr -> String
-- aExToC (Var x) = x
-- aExToC (Const x) = show x
-- aExToC (Add x y) = putParen x ++ " + " ++ putParen y
-- aExToC (Sub x y) = putParen x ++ " - " ++ putParen y
-- aExToC (Div x y) = putParen x ++ " / " ++ putParen y
-- aExToC (FCall x y) = callBuilder x y

-- -- Build a function call with list of AExpr as arguments
-- callBuilder :: Funcs -> [AExpr] -> String 
-- callBuilder (_,y,_,_) [a] = y ++ "(" ++ aExToC a ++ ")"
-- callBuilder (_,y,_,_) (a:as) = y ++ "(" ++ aExToC a ++ "," ++ aExListToC as ++ ")"

-- -- List arguments in object instantiation (WE SHOULD STORE ARGUMENTS WITH QUOTES IF THEY ARE STRINGS)
-- argsLister :: [Arguments] -> String 
-- argsLister [x] = x
-- argsLister (x:xs) = x ++ ", " ++ argsLister xs
-- ---------------------------------------------------------------------------------------
-- paramLister :: [Params] -> String
-- paramLister [] = ""
-- paramLister ((varType, varName):xs) | xs /= []  = dataConv varType ++ " " ++ varName ++ "," ++ paramLister xs
--                                     | otherwise = dataConv varType ++ " " ++ varName

-- functionLister :: [Funcs] -> String
-- functionLister ((return, name, params, states):xs) = dataConv return ++ " " ++ name ++ "(" ++ paramLister params ++ ")" ++ "{" ++ statementLister states ++ "}"
-- ----------------------------------------------------------------------------------------
-- -- Pass name and list of functions to be built
-- funcyListBuilder :: CName -> [Funcs] -> String
-- funcyListBuilder x [y] = funcyBuilder x y
-- funcyListBuilder x (y:ys) = funcyBuilder x y ++ funcyListBuilder x ys

-- -- Build functions and constructor for simulated class
-- {- When saving statements in constructor we need to save the variable names with obj.name like newStudent.name -}
-- funcyBuilder :: CName -> Funcs -> String
-- funcyBuilder n (w,x,y,z) | n == w && n == x = "struct " ++ n ++ " " ++ n ++ "_" ++ n ++ "(" ++ paramLister y ++ ")" ++ "{" ++
--                                     "struct " ++ n ++ " new" ++ n ++ "; " ++ statementLister z ++ " return " ++ " new" ++ n ++ ";"
--                          | otherwise = dataConv w ++ " " ++ x ++ "(" ++ paramLister y ++ ")" ++ "{" ++ statementLister z ++ "}"


-- [CDec ("Student",FullCFunct ("Student",[(Str,"n"),(Int,"a")],
--  [ReAssign "name" "n",ReAssign "age" "a"]),[(Int,"age"),(Str,"name")],[(Str,"getName",[],[Ret (Var "name")]),(Vid,"setName",[(Str,"n")],[TReAssign "name" "n"]),(Int,"getAge",[],[Ret (Var "age")]),(Vid,
--  "setAge",[(Int,"a")],[TReAssign "age" "a"])])

--  DDec [OInst ("Student","Jacob",["\"Jacob\"","100"]),OInst ("Student","Wolfe",["\"Wolfe\"","999"]),
--   OInst ("Student","Luke",["\"Luke\"","2"]),ObjFCall ("Jacob","setAge",Sub (AeObjCall ("Wolfe","getAge",[])) (AeObjCall
--  ("Luke","getAge",[]))),ObjectPrint ("Jacob","getAge",[])]]


-- -------------------------------TEST-----------------------------------------------------
-- states1 = [Assign "int" "X" (Left (Const 1)), 
--            ReAssign "X" (Left (Add (Const 60) (Sub (Const 10) (Var "X"))))]
-- functs1 :: [Funcs]
-- functs1 = [("int", "addOne", [("int","x")],
--           [ReAssign "X" (Left (Add (Var "x") (Const 1)))])]

-- states2 = [Assign "int" "Y" (Left (Const 5))]

-- functs2 = [("int", "getY", [], [Ret (Var "Y")])]

-- test1 = DDec ("main", states1, functs1)
-- test2 = CDec ("Student", states2, functs2)

-- concreteTest1 = cDriver test1
-- concreteTest2 = cDriver test2
-- -------------------------------TEST-----------------------------------------------------
