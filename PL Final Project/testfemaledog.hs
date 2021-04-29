-- foo :: String -> String
-- foo ('P':'u':'b':'l':'i':'c':' ':'C':'l':'a':'s':'s':' ': xs) = 


halts = " ([{,)]}"
getWord' :: String -> String
getWord' [] = []
getWord' (x:xs) | x `elem` halts = []
               | otherwise = x:getWord' xs

getScuffed :: String -> (String, String)
getScuffed [] = ([], [])
getScuffed (x:xs) | x `elem` halts = ([], xs)
                  | otherwise = (x:ys, zs)
                    where (ys, zs) = getScuffed xs

getParam :: String -> [Params]
getParam [] = []
getParam (x:xs) | x == ')' = []
                | otherwise = (tpe, var):getParam str2
                      where (var, str2) = getScuffed str1
                            (tpe, str1) = getScuffed xs

-- testfuck :: String -> Bool
-- testfuck str@("fuck":"what") = reverse str
-- testfuck _ = str


type Params = (PType, PVar)
type PType   = String
type PVar    = String







------------------------------------------------------------------------------------
cDriver :: DDec -> String
cDriver (x, y, z) = "int main() {" ++ statementLister y ++ FuncsLister z ++ "}"
--(DName, [Statements], [Funcs])
-- data Statements = Assign VType Vars (Either AExpr BExpr) -- variable assignments
--             | Declare CVars -- class level variables
--             | Print AExpr -- print statements
--             | Ret AExpr -- return statement
--             | ODec Obj -- object instantiation
--type Funcs = (RetType, FName, [Params], [Statements])