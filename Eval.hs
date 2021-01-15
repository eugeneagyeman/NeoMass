module Eval where
import           Grammar
import           System.Environment
import           System.IO
import           Data.Function
import           Data.List
import           Control.Monad.Trans.Writer
-- Evaluator for NeoMass

--Data Structure Defined in Grammar
--data Type = TypeInt | TypeVar
--type Environment = [(String, Type, Expr)]
{- 
Program : StatementSeq                  { Program $1 }

Statement : ReturnStatement             { ReturnStmt $1 }
          | ConditionStatement          { ConditionStmt $1 }
          | ExpressionStatement         { ExpressionStmt $1 }
          | AssignIDStatement           { AssignStmt $1 }
          | PrintStatement              { PrintStmt $1 }
          | WhileStatement              { WhileStmt $1 }

StatementSeq : Statement ";" StatementSeq   { [$1] ++ $3 }
             |                              { [] }

VarDeclareStatement : Type id                                     { VarID $1 $2 }
                    | Type bracket id                             { VarArrayID $1 $3 }
                    | id                                          { VarReference $1 }

ReturnStatement : "return" Expr                                     { ReturnExpr $2 }

ConditionStatement : "if" "(" Expr ")" "{" StatementSeq "}"         { IfStatement $3 $6 }
                   | "if" "(" Expr ")" "{" StatementSeq "}" "else" "{" StatementSeq "}"    {IfElseStatement $3 $6 $10}

ExpressionStatement : Expr                           { ExprStmt $1 }

AssignIDStatement   : VarDeclareStatement "=" Expr   { AssignID $1 $3 }
                    | "null"                         { NullLiteral }
                    | VarDeclareStatement "++"      { AssignID $1 (Add (Var $1) (IntegerLiteral 1))}


PrintStatement : "print" Expr                        { PrintExpr $2 }

WhileStatement : "while" "(" Expr ")" "{" StatementSeq "}"    { WhileExprStatement $3 $6 }
               | "loop" Expr "{" StatementSeq "}"             { LoopStream $2 $4 }

ArrayList : ArrayList "," Expr {($1 ++ [$3])}
          | Expr               {[$1]}
          |                    { [] }

Expr: Expr "+" Expr  { Add $1 $3 }
    | Expr "-" Expr  { Minus $1 $3 }
    | Expr "*" Expr  { Multiply $1 $3 }
    | Expr "/" Expr  { Div $1 $3 }
    | Expr ">" Expr  { GreaterThan $1 $3 }
    | Expr ">=" Expr { GreaterEqual $1 $3 }
    | Expr "<" Expr  { LessThan $1 $3 }
    | Expr "<=" Expr { LessThanEqual $1 $3 }
    | Expr "==" Expr { Equality $1 $3 }
    | Expr "&&" Expr { And $1 $3 }
    | Expr "||" Expr { Or $1 $3 }
    | Expr "!=" Expr { NotEqual $1 $3 }
    | "!" Expr       { Not $2 }
    | "-" Expr       { Negate $2 }
    | "(" Expr ")"   { $2 }
    | "[" ArrayList "]" { Array $2 }
    | STRINGLIT      { StringLiteral $1 }
    | INTLIT         { IntegerLiteral $1 }  
    | "true"         { ExprBool True  }
    | "false"        { ExprBool False }
    | VarDeclareStatement   { Var $1 }
    | input          { ExprInputStreams }
    | Expr ".length"     { ExprLength $1 }
    | Expr readStream "(" Expr "," Expr")" { ExprGetStreamElement $1 $4 $6}
    | Expr readStream "(" Expr ")"         { ExprGetStream $1 $4 }
    | Expr ".isEmpty"     { ExprIsEmpty $1 }
    

Type: "int"          { TypeInt }
    | "var"          { TypeVar }
{

parseError :: [Token] -> a
parseError [] = error " Unknown parse error"
parseError (x:xs) = error ("Parse error at line:column " ++ (tokenPosn x))

data Program = Program StatementSeq
     deriving (Show,Eq)

data Statement = ReturnStmt ReturnStatement
        | ConditionStmt ConditionStatement
        | ExpressionStmt ExpressionStatement
        | AssignStmt AssignIDStatement
        | PrintStmt PrintStatement
        | WhileStmt WhileStatement
        | ErrorStmt
        deriving (Show, Eq)

data VarDeclareStatement = VarID Type String
                  | VarArrayID Type String
                  | VarReference String
                  deriving (Show,Eq)

data ReturnStatement = ReturnExpr Expr
                     deriving (Show,Eq)

data ConditionStatement = IfStatement Expr StatementSeq
                        | IfElseStatement Expr StatementSeq StatementSeq
                        deriving (Show,Eq)

data ExpressionStatement = ExprStmt Expr
                         deriving (Show,Eq)

data AssignIDStatement = AssignID VarDeclareStatement Expr
                        | NullLiteral

                      deriving (Show, Eq)

data PrintStatement   = PrintExpr Expr
                      | PrintVar String
                      deriving (Show, Eq)

data WhileStatement   = WhileExprStatement Expr StatementSeq
                      | LoopStream Expr StatementSeq
                      deriving (Show, Eq)

data Expr   = Add Expr Expr
            | Minus Expr Expr
            | Multiply Expr Expr
            | Div Expr Expr
            | GreaterThan Expr Expr
            | GreaterEqual Expr Expr
            | LessThan Expr Expr
            | LessThanEqual Expr Expr
            | Equality Expr Expr
            | NotEqual Expr Expr
            | And Expr Expr
            | Or Expr Expr
            | Not Expr
            | Negate Expr 
            | Array ArrayList
            | StringLiteral String
            | IntegerLiteral Int
            | ExprBool Bool
            | Var VarDeclareStatement
            | ExprInputStreams
            | ExprLength Expr
            | Streams Input
            | ExprGetStreamElement Expr Expr Expr
            | ExprGetStream Expr Expr
            | ExprIsEmpty Expr
            | UNASSIGNED
            deriving (Show, Eq)

type ArrayList = [Expr]
type Input = [ArrayList]
type ID = String
type StatementSeq = [Statement]
type VarIDStatement = String

data Type = TypeInt
          | TypeVar
          | TypeIntArray
          deriving (Show,Eq)

type BracketList = Token
type Environment = [(String,Type,Expr)]
-}

isValueFree :: String -> [Environment] -> Bool
isValueFree x [] = True
isValueFree x (e : env) | allEnvs   = isValueFree x env
                        | otherwise = False
  where allEnvs = freeVarinEnv x e

freeVarinEnv :: String -> Environment -> Bool
freeVarinEnv x [] = True
freeVarinEnv x ((y, t, e) : env) | x == y    = False
                                 | otherwise = freeVarinEnv x env

isUnassigned :: Expr -> Bool
isUnassigned (UNASSIGNED) = True
isUnassigned _            = False

-- Checks for terminated expressions
isValue :: Expr -> Bool
isValue (IntegerLiteral _) = True
isValue (StringLiteral  _) = True
isValue (ExprBool       _) = True
isValue _                  = False

isTrue :: Expr -> Bool
isTrue (ExprBool True) = True
isTrue _               = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _       = False

isEmpty :: Expr -> Bool
isEmpty (Array   []  ) = True
isEmpty (Streams [[]]) = True
isEmpty _              = False

evaluateProgram :: Program -> [[Int]] -> IO ([Environment])
evaluateProgram (Program ss) input = do
  evaluatedStatements <- evaluateStatementSeq ss input [[]]
  return (evaluatedStatements)

evaluateStatementSeq
  :: [Statement] -> [[Int]] -> [Environment] -> IO ([Environment])
evaluateStatementSeq []       input env = return env
evaluateStatementSeq (s : ss) input env = do
  newEnv <- evaluateStatement s input env
  evaluateStatementSeq ss input newEnv

evaluateStatement :: Statement -> [[Int]] -> [Environment] -> IO ([Environment])
evaluateStatement (ExpressionStmt (ExprStmt (Var v))) input env =
  return (addBinding (getVarName v) (getVarType v) (UNASSIGNED) env)
evaluateStatement (ExpressionStmt (ExprStmt e)) input env = do
  let returnedExpr = evaluateExpr e input env
  return env

evaluateStatement (AssignStmt (AssignID v e)) input env = do
  let returnedExpr      = evaluateExpr e input env
      assignedStatement = assignVariable (AssignID v returnedExpr) env
  return assignedStatement
evaluateStatement (ReturnStmt (ReturnExpr (Var v))) input env = do
  let returnedExpr = evaluateExpr (lookupVariable v env) input env
  putStrLn (prettyPrint returnedExpr)
  return env

evaluateStatement (ReturnStmt (ReturnExpr e)) input env = do
  let returnedExpr = evaluateExpr e input env
  putStrLn (prettyPrint returnedExpr)
  return env

evaluateStatement (ConditionStmt (IfStatement condition seq)) input env = do
  let returnedExpr = evaluateExpr condition input env
  if isTrue returnedExpr
    then do
      evaluatedStatements <- evaluateStatementSeq seq input env
      return (tail (evaluatedStatements))
    else return env

evaluateStatement (ConditionStmt (IfElseStatement condition tseq fseq)) input env
  = do
    let returnedExpr = evaluateExpr condition input env
    if isTrue returnedExpr
      then do
        trueStatements <- evaluateStatementSeq tseq input ([] : env)
        return (tail (trueStatements))
      else do
        falseStatements <- evaluateStatementSeq fseq input ([] : env)
        return (tail (falseStatements))

evaluateStatement (WhileStmt (WhileExprStatement condition seq)) input env = do
  let returnedExpr = evaluateExpr condition input env
  if isTrue returnedExpr
    then do
      evaluatedStatements <- evaluateStatementSeq seq input ([] : env)
      evaluateStatement (WhileStmt (WhileExprStatement condition seq))
                        input
                        (tail (evaluatedStatements))
    else return env

evaluateStatement (WhileStmt (LoopStream (Var v) seq)) input env = do
  let returnedExpr = evaluateExpr
        (ExprBool (isEmpty (ExprLength (lookupVariable v env))))
        input
        env
  if isTrue returnedExpr
    then do
      evaluatedStatements <- evaluateStatementSeq seq input ([] : env)
      evaluateStatement (WhileStmt (LoopStream (Var v) seq))
                        input
                        (tail (evaluatedStatements))
    else return env

evaluateStatement (PrintStmt (PrintExpr expr)) input env = do
  let returnedExpr = evaluateExpr expr input env
  putStr (prettyPrint returnedExpr)
  return env

assignVariable :: AssignIDStatement -> [Environment] -> [Environment]
assignVariable (AssignID var exp) env
  | isValueFree (getVarName var) env == False = updateVariable var exp env
  | otherwise = addBinding (getVarName var) (getVarType var) exp env

updateVariable :: VarDeclareStatement -> Expr -> [Environment] -> [Environment]
updateVariable _ _ [] = error ("Variable cannot be found")
updateVariable var expr (e : env)
  | snd allEnvs == False = e : updateVariable var expr env
  | otherwise            = (fst allEnvs) : env
  where allEnvs = updateVariableInEnv (getVarName var) expr e

updateVariableInEnv :: String -> Expr -> Environment -> (Environment, Bool)
updateVariableInEnv _ _ [] = ([], False)
updateVariableInEnv ref updateExpr ((str, typ, exp) : env)
  | ref == str = ((str, typ, updateExpr) : env, True)
  | ref /= str = ((str, typ, exp) : (fst nextOne), snd nextOne)
  where nextOne = updateVariableInEnv ref updateExpr env

lookupVariable :: VarDeclareStatement -> [Environment] -> Expr
lookupVariable _ [] = error "No Variables"
lookupVariable (VarReference ref) (e : env)
  | isUnassigned lookup = lookupVariable (VarReference ref) env
  | otherwise           = lookup
  where lookup = lookUpVarInEnv ref e
lookupVariable (VarID typ id) (e : env)
  | isUnassigned lookup = lookupVariable (VarID typ id) env
  | otherwise           = lookup
  where lookup = lookUpVarInEnv id e
lookupVariable (VarArrayID typ name) (e : env)
  | isUnassigned lookup = lookupVariable (VarArrayID typ name) env
  | otherwise           = lookup
  where lookup = lookUpVarInEnv name e

lookUpVarInEnv :: String -> Environment -> Expr
lookUpVarInEnv _ [] = UNASSIGNED
lookUpVarInEnv ref ((s, t, e) : env)
  | ref == s && isUnassigned e == False = e
  | ref == s && isUnassigned e = error
    ("getBoundInEnv: Variable '" ++ ref ++ "' uninitialised.")
  | otherwise = lookUpVarInEnv ref env

getVarName :: VarDeclareStatement -> String
getVarName (VarReference ref) = ref
getVarName (VarID      _ id ) = id
getVarName (VarArrayID _ arr) = arr

getVarType :: VarDeclareStatement -> Type
getVarType (VarID      typ _) = typ
getVarType (VarArrayID t   _) = t
getVarType (VarReference _  ) = TypeVar


addBinding :: String -> Type -> Expr -> [Environment] -> [Environment]
addBinding var typ exp env = ((var, typ, exp) : (head env)) : tail env

trace :: String -> Writer String () -> Writer String ()
trace name f = do
  tell $ "Entering " ++ name ++ "\n"
  f
  tell $ "Leaving " ++ name ++ "\n"

evaluateExpr :: Expr -> [[Int]] -> [Environment] -> Expr
evaluateExpr (Add (Var v1) (Var v2)) input env =
  evaluateExpr (Add (lookupVariable v1 env) (lookupVariable v2 env)) input env
evaluateExpr (Minus (Var v1) (Var v2)) input env =
  evaluateExpr (Minus (lookupVariable v1 env) (lookupVariable v2 env)) input env
evaluateExpr (Multiply (Var v1) (Var v2)) input env = evaluateExpr
  (Multiply (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (Div (Var v1) (Var v2)) input env =
  evaluateExpr (Div (lookupVariable v1 env) (lookupVariable v2 env)) input env
evaluateExpr (GreaterThan (Var v1) (Var v2)) input env = evaluateExpr
  (GreaterThan (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (GreaterEqual (Var v1) (Var v2)) input env = evaluateExpr
  (GreaterEqual (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (LessThan (Var v1) (Var v2)) input env = evaluateExpr
  (LessThan (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (LessThanEqual (Var v1) (Var v2)) input env = evaluateExpr
  (LessThanEqual (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (Equality (Var v1) (Var v2)) input env = evaluateExpr
  (Equality (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (NotEqual (Var v1) (Var v2)) input env = evaluateExpr
  (NotEqual (lookupVariable v1 env) (lookupVariable v2 env))
  input
  env
evaluateExpr (And (Var v1) (Var v2)) input env =
  evaluateExpr (And (lookupVariable v1 env) (lookupVariable v2 env)) input env
evaluateExpr (Or (Var v1) (Var v2)) input env =
  evaluateExpr (Or (lookupVariable v1 env) (lookupVariable v2 env)) input env
evaluateExpr (Not (Var v1)) input env =
  evaluateExpr (Not (lookupVariable v1 env)) input env
evaluateExpr (Negate (Var v1)) input env =
  evaluateExpr (Negate (lookupVariable v1 env)) input env
evaluateExpr (Add exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateAddExpr exp1 exp2
  | otherwise                    = evaluateAddExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env
evaluateExpr (Minus exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateMinusExpr exp1 exp2
  | otherwise                    = evaluateMinusExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (Div exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateDivExpr exp1 exp2
  | otherwise                    = evaluateDivExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (Multiply exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateMultiplyExpr exp1 exp2
  | otherwise = evaluateMultiplyExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (GreaterThan exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateGreaterThanExpr exp1 exp2
  | otherwise = evaluateGreaterThanExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (GreaterEqual exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateGreaterEqualExpr exp1 exp2
  | otherwise = evaluateGreaterEqualExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (LessThan exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateLessThanExpr exp1 exp2
  | otherwise = evaluateLessThanExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (LessThanEqual exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateLessEqualExpr exp1 exp2
  | otherwise = evaluateLessEqualExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (Equality exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateEqualityExpr exp1 exp2
  | otherwise = evaluateEqualityExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (NotEqual exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateNotEqualExpr exp1 exp2
  | otherwise = evaluateNotEqualExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env
evaluateExpr (And exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateAndExpr exp1 exp2
  | otherwise                    = evaluateAndExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (Or exp1 exp2) input env
  | isValue exp1 && isValue exp2 = evaluateOrExpr exp1 exp2
  | otherwise                    = evaluateOrExpr evaluatedExp1 evaluatedExp2

 where
  evaluatedExp1 = evaluateExpr exp1 input env
  evaluatedExp2 = evaluateExpr exp2 input env

evaluateExpr (Not exp1) input env | isValue exp1 = evaluateNotExpr exp1
                                  | otherwise    = evaluateNotExpr evaluatedExp1
  where evaluatedExp1 = evaluateExpr exp1 input env

evaluateExpr (Negate exp1) input env
  | isValue exp1 = evaluateNegateExpr exp1
  | otherwise    = evaluateNegateExpr evaluatedExp1
  where evaluatedExp1 = evaluateExpr exp1 input env
evaluateExpr (Var            v1 ) input env = lookupVariable v1 env
evaluateExpr (ExprBool       b  ) input env = (ExprBool b)
evaluateExpr (Array          a  ) input env = (Array a)
evaluateExpr (StringLiteral  str) input env = (StringLiteral str)
evaluateExpr (IntegerLiteral l1 ) input env = (IntegerLiteral l1)
evaluateExpr (ExprLength e) input env | isValue e = evaluateLengthExpr e
                                      | otherwise = evaluateLengthExpr evalExpr
  where evalExpr = evaluateExpr e input env
evaluateExpr (ExprInputStreams) input env = convertInputStreams input
evaluateExpr (ExprGetStream seq elementIndex) input env
  | isValue seq && isValue elementIndex = getSingularStream seq elementIndex
  | otherwise = getSingularStream returnedVar returnedIndex
 where
  returnedVar   = evaluateExpr seq input env
  returnedIndex = evaluateExpr elementIndex input env
evaluateExpr (ExprGetStreamElement seq streamIndex elementIndex) input env
  | isValue seq && isValue streamIndex && isValue elementIndex
  = getSingularStreamElement seq streamIndex elementIndex
  | otherwise
  = getSingularStreamElement returnedVar returnedStreamIndex returnedElemIndex

 where
  returnedVar         = evaluateExpr seq input env
  returnedStreamIndex = evaluateExpr streamIndex input env
  returnedElemIndex   = evaluateExpr elementIndex input env
evaluateExpr (ExprIsEmpty expr) input env = ExprBool (isEmpty expr)
evaluateExpr e1                 input env = error
  (  "Interpreter.EvaluateExpr Error: Could not evaluate the expression"
  ++ show e1
  )

getSingularStream :: Expr -> Expr -> Expr
getSingularStream (Array   []  ) _ = Array []
getSingularStream (Streams [[]]) _ = Array []
getSingularStream (Array arr) (IntegerLiteral index)
  | index <= length arr = arr !! index
  | otherwise           = error "Index larger than size of list"
getSingularStream (Streams streams) (IntegerLiteral index)
  | index <= length streams = Array (streams !! index)
  | otherwise               = error "Index larger than size of list"
getSingularStream e i = error
  (  "getSingularStream:Could not get the element in list: "
  ++ show e
  ++ " : "
  ++ show i
  )

getSingularStreamElement :: Expr -> Expr -> Expr -> Expr
getSingularStreamElement (Array   []  ) _ _ = Array []
getSingularStreamElement (Streams [[]]) _ _ = Array []
getSingularStreamElement (Array arr) _ (IntegerLiteral index)
  | index <= length arr = arr !! index
  | otherwise           = error "Index larger than size of list"
getSingularStreamElement (Streams streams) (IntegerLiteral streamIndex) (IntegerLiteral elementIndex)
  | streamIndex <= length streams && elementIndex <= length
    (streams !! streamIndex)
  = (streams !! streamIndex) !! elementIndex
  | otherwise
  = error ("error in indexing")

convertInputStreams :: [[Int]] -> Expr
convertInputStreams []    = Streams [[]]
convertInputStreams input = Streams (map (map IntegerLiteral) input)
convertInputStreams _     = error
  ("convertInputStreams: Streams are malformed - please look at your input")

evaluateAddExpr :: Expr -> Expr -> Expr
evaluateAddExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  IntegerLiteral (l1 + l2)
evaluateAddExpr (Array arr1) (Array arr2) = Array (arr1 ++ arr2)
--To add, type checking for arrays.

evaluateMinusExpr :: Expr -> Expr -> Expr
evaluateMinusExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  IntegerLiteral (l1 - l2)

evaluateMultiplyExpr :: Expr -> Expr -> Expr
evaluateMultiplyExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  IntegerLiteral (l1 * l2)
evaluateMultiplyExpr e1 e2 = error
  (  "evaluateMultiplyExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )


evaluateDivExpr :: Expr -> Expr -> Expr
evaluateDivExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  IntegerLiteral (l1 `div` l2)
evaluateDivExpr e1 e2 = error
  (  "evaluateDivExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )

evaluateGreaterThanExpr :: Expr -> Expr -> Expr
evaluateGreaterThanExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  ExprBool (l1 > l2)
evaluateGreaterThanExpr (StringLiteral s1) (StringLiteral s2) =
  ExprBool (s1 > s2)
evaluateGreaterThanExpr (Array arr) (Array arr1) = evaluateGreaterThanExpr
  (evaluateLengthExpr (Array arr))
  (evaluateLengthExpr (Array arr1))

evaluateGreaterThanExpr e1 e2 = error
  (  "evaluateGreaterThanExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )

evaluateLessThanExpr :: Expr -> Expr -> Expr
evaluateLessThanExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  ExprBool (l1 < l2)
evaluateLessThanExpr (StringLiteral s1) (StringLiteral s2) = ExprBool (s1 < s2)
evaluateLessThanExpr (Array arr) (Array arr1) = evaluateLessThanExpr
  (evaluateLengthExpr (Array arr))
  (evaluateLengthExpr (Array arr1))
evaluateLessThanExpr e1 e2 = error
  (  "evaluateLessThanExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )


evaluateGreaterEqualExpr :: Expr -> Expr -> Expr
evaluateGreaterEqualExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  ExprBool (l1 >= l2)
evaluateGreaterEqualExpr (StringLiteral s1) (StringLiteral s2) =
  ExprBool (s1 >= s2)
evaluateGreaterEqualExpr (Array arr) (Array arr1) = evaluateGreaterEqualExpr
  (evaluateLengthExpr (Array arr))
  (evaluateLengthExpr (Array arr1))
evaluateGreaterEqualExpr e1 e2 = error
  (  "evaluateGreaterThanExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )


evaluateLessEqualExpr :: Expr -> Expr -> Expr
evaluateLessEqualExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  ExprBool (l1 <= l2)
evaluateLessEqualExpr (StringLiteral s1) (StringLiteral s2) =
  ExprBool (s1 <= s2)
evaluateLessEqualExpr (Array arr) (Array arr1) = evaluateLessEqualExpr
  (evaluateLengthExpr (Array arr))
  (evaluateLengthExpr (Array arr1))
evaluateLessEqualExpr e1 e2 = error
  (  "evaluateLessEqualExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )


evaluateEqualityExpr :: Expr -> Expr -> Expr
evaluateEqualityExpr (ExprBool e1) (ExprBool e2) = ExprBool (e1 == e2)
evaluateEqualityExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  ExprBool (l1 == l2)
evaluateEqualityExpr (StringLiteral s1) (StringLiteral s2) =
  ExprBool (s1 == s2)
evaluateEqualityExpr e1 e2 = error
  (  "evaluateEqualityExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )

evaluateNotEqualExpr :: Expr -> Expr -> Expr
evaluateNotEqualExpr (ExprBool e1) (ExprBool e2) = ExprBool (e1 /= e2)
evaluateNotEqualExpr (IntegerLiteral l1) (IntegerLiteral l2) =
  ExprBool (l1 /= l2)
evaluateNotEqualExpr (StringLiteral s1) (StringLiteral s2) =
  ExprBool (s1 /= s2)
evaluateNotEqualExpr e1 e2 = error
  (  "evaluateNotEqualExpr: Cannot compare these expressions "
  ++ show e1
  ++ ":"
  ++ show e2
  )

evaluateAndExpr :: Expr -> Expr -> Expr
evaluateAndExpr (ExprBool e1) (ExprBool e2) = ExprBool (e1 && e2)
evaluateAndExpr _ _ =
  error "evaluateAndExpr: Cannot compare non boolean expressions"

evaluateOrExpr :: Expr -> Expr -> Expr
evaluateOrExpr (ExprBool e1) (ExprBool e2) = ExprBool (e1 || e2)
evaluateOrExpr _ _ =
  error "evaluateOrExpr: Cannot compare non boolean expressions"

evaluateNotExpr :: Expr -> Expr
evaluateNotExpr (ExprBool b1) = ExprBool (not b1)
evaluateNotExpr e             = error
  ("evaluateNotExpr: Not cannot be evaluated on this expression" ++ show e)

evaluateNegateExpr :: Expr -> Expr
evaluateNegateExpr (IntegerLiteral l1) = IntegerLiteral (-l1)
evaluateNegateExpr (ExprBool b1) = ExprBool (not b1)
evaluateNegateExpr e = error ("evaluateNegateExpr: Cannot negate:  " ++ show e)

evaluateLengthExpr :: Expr -> Expr
evaluateLengthExpr (Array         arr    ) = IntegerLiteral (length arr)
evaluateLengthExpr (Streams       streams) = IntegerLiteral (length streams)
evaluateLengthExpr (StringLiteral str    ) = IntegerLiteral (length str)
evaluateLengthExpr e =
  error ("evaluateLengthExpr: Cannot evaluate the length of:  " ++ show e)

prettyPrint :: Expr -> String
prettyPrint (IntegerLiteral i1 ) = show i1 ++ "\n"
prettyPrint (StringLiteral  x1 ) = show x1 ++ "\n"
prettyPrint (ExprBool       b1 ) = show b1 ++ "\n"
prettyPrint (Array          arr) = show (map show arr) ++ "\n"
prettyPrint e                    = show e ++ "\n"
