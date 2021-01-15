{
module Grammar where
import Tokens
}

%name parseProg
%tokentype { Token }
%error { parseError }
%token
    "+"        { TokenPlus _ }
    "-"        { TokenMinus _ }
    "/"        { TokenDiv _ }
    "*"        { TokenTimes _ }
    "!="       { TokenNotEqual _ }
    "="        { TokenEq _ }
    "=="       { TokenEquality _ }
    "<"        { TokenLessThan _ }
    "<="       { TokenLessEqual _ }
    ">="       { TokenMoreEqual _ }
    ">"        { TokenMoreThan _ }
    "("        { TokenLParen _ }
    ")"        { TokenRParen _ }
    "{"        { TokenLSquigParen _ }
    "}"        { TokenRSquigParen _ }
    ";"        { TokenSemicolon _ }
    "true"     { TokenTrue _ }
    "false"    { TokenFalse _ }
    "if"       { TokenIf _ }
    "else"     { TokenElse _ }
    "while"    { TokenWhile _ }
    "int"      { TokenIntDeclare _ }
    "return"   { TokenReturn _ }
    "print"    { TokenPrint _ }
    ".length"  { TokenLength _ }
    "null"     { TokenNull _ }
    id         { TokenVar _ $$}
    bracket    { TokenBracket _ _}
    STRINGLIT  { TokenStringLit _ $$ }
    INTLIT     { TokenInt _ $$}
    "&&"       { TokenAnd _ }
    "||"       { TokenOr _ }
    "!"        { TokenNot _ }
    "["        { TokenLSquareBracket _ }
    "]"        { TokenRSquareBracket _ }
    ","        { TokenColon _ }
    "var"      { TokenVarDeclare _ }
    input      { TokenInputDeclare _ }
    readStream   { TokenReadStream _   }
    "loop"       { TokenWhileLoopDeclare _ }
    ".isEmpty"   { TokenIsEmpty _}
    "++"         { TokenIncrement _}

%right ";"
%right "="
%left  "||"
%left  "&&"
%left "==" "!="
%left "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/"
%right "!" "-"
%nonassoc "if" "while" "loop"
%nonassoc "else"
%nonassoc '(' ')' bracket '[' ']' "null" 
%nonassoc "print" "int" "var" "return" ".length" readStream ".isEmpty"

%%

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
}