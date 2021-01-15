{
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$white = [\ \t\f\v\r \n]
$graphic    = $printable
@string     = \" ($graphic # \")*\"

tokens :-

$white+     ; 
  "!!".*    ;
  \!=             { tok (\p s -> TokenNotEqual p) }
  \=              { tok (\p s -> TokenEq p)       }
  \=\=            { tok (\p s -> TokenEquality p) }
  \<              { tok (\p s -> TokenLessThan p) }
  \<=             { tok (\p s -> TokenLessEqual p)}
  \>              { tok (\p s -> TokenMoreThan p) }
  \>=             { tok (\p s -> TokenMoreEqual p)}
  \+              { tok (\p s -> TokenPlus p)     }
  \-              { tok (\p s -> TokenMinus p)    } 
  \*              { tok (\p s -> TokenTimes p)    }
  \/              { tok (\p s -> TokenDiv p)      }
  \(              { tok (\p s -> TokenLParen p)   }
  \)              { tok (\p s -> TokenRParen p)   }
  \{              { tok (\p s -> TokenLSquigParen p) }
  \}              { tok (\p s -> TokenRSquigParen p) }
  \;              { tok (\p s -> TokenSemicolon p) }
  \!              { tok (\p s -> TokenNot p)}    
  true            { tok (\p s -> TokenTrue p)}
  false           { tok (\p s -> TokenFalse p)}
  if              { tok (\p s -> TokenIf p)}
  else            { tok (\p s -> TokenElse p)}
  while           { tok (\p s -> TokenWhile p)}
  \[              { tok (\p s -> TokenLSquareBracket p)}
  \]              { tok (\p s -> TokenRSquareBracket p)}
  \,              { tok (\p s -> TokenColon p)}
  "[]"+           { tok (\p s -> TokenBracket p (countBrackets s))}
  int             { tok (\p s -> TokenIntDeclare p)}
  var             { tok (\p s -> TokenVarDeclare p)}
  return          { tok (\p s -> TokenReturn p)}
  print           { tok (\p s -> TokenPrint p )}
  [n N]{1} ull    { tok (\p s -> TokenNull p)}
  \.length        { tok (\p s -> TokenLength p)}
  &&              { tok (\p s -> TokenAnd p)}
  \|{2}           { tok (\p s -> TokenOr p)}
  $digit+         { tok (\p s -> TokenInt p (read s)) }
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) }
  @string         { tok (\p s -> TokenStringLit p (init (tail s))) }
  "[[]]"          { tok (\p s -> TokenInputDeclare p)}
  \:\:            { tok (\p s -> TokenReadStream p)}
  "loop"          { tok (\p s -> TokenWhileLoopDeclare p)}
  \.isEmpty       { tok (\p s -> TokenIsEmpty p)}
  \+{2}           { tok (\p s -> TokenIncrement p)}

{
--Action helper
tok f p s = f p s

countBrackets :: String -> Int
countBrackets = length . filter (=='[')

-- The token type: 
data Token = 
  TokenInt AlexPosn Int       |
  TokenNot AlexPosn           | 
  TokenVar AlexPosn String    | 
  TokenEq AlexPosn            |
  TokenPlus AlexPosn          |
  TokenMinus AlexPosn         |
  TokenTimes AlexPosn         |
  TokenDiv AlexPosn           |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenIntDeclare AlexPosn    |
  TokenBracket AlexPosn Int   |
  TokenLSquareBracket AlexPosn|
  TokenRSquareBracket AlexPosn|
  TokenColon AlexPosn         |
  TokenTrue AlexPosn          |
  TokenFalse AlexPosn         |
  TokenIf AlexPosn            | 
  TokenElse AlexPosn          |
  TokenLSquigParen AlexPosn   |
  TokenRSquigParen AlexPosn   |
  TokenSemicolon AlexPosn     |
  TokenWhile AlexPosn         |
  TokenReturn AlexPosn        |
  TokenPrint AlexPosn         |
  TokenNotEqual AlexPosn      |
  TokenLessThan AlexPosn      |
  TokenLessEqual AlexPosn     |
  TokenMoreThan AlexPosn      |
  TokenMoreEqual AlexPosn     |
  TokenNull AlexPosn          |
  TokenLength AlexPosn        |
  TokenEquality AlexPosn      |
  TokenAnd AlexPosn           |
  TokenOr AlexPosn            |
  TokenVarDeclare AlexPosn    |
  TokenInputDeclare AlexPosn  |
  TokenReadStream AlexPosn    |
  TokenWhileLoopDeclare AlexPosn |
  TokenIsEmpty AlexPosn   |
  TokenIncrement AlexPosn |
  TokenStringLit AlexPosn String
      deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenInt (AlexPn o l c) i)    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn o l c) v)    = show(l) ++ ":" ++ show(c)  
tokenPosn (TokenEq (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntDeclare (AlexPn o l c))       = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBracket (AlexPn o l c) p)     = show(l) ++ ":" ++ show(c) ++ "streamlength: " ++ show(p)
tokenPosn (TokenTrue (AlexPn o l c))    = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenFalse (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn o l c))    = show(l) ++ ":" ++ show(c) 
tokenPosn (TokenElse (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquigParen (AlexPn o l c))      = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquigParen (AlexPn o l c))      = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLSquareBracket (AlexPn o l c))      = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRSquareBracket (AlexPn o l c))      = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemicolon (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn o l c))      = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPrint (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEqual (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessEqual (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMoreEqual (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMoreThan (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNull (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquality (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn o l c))    =   show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn o l c))     =   show(l) ++ ":" ++ show(c)
tokenPosn (TokenVarDeclare (AlexPn o l c))     =   show(l) ++ ":" ++ show(c)
tokenPosn (TokenStringLit (AlexPn o l c) s)    =   show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn o l c))    = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInputDeclare (AlexPn o l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReadStream (AlexPn o l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhileLoopDeclare (AlexPn o l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIsEmpty (AlexPn o l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIncrement (AlexPn o l c)) = show(l) ++ ":" ++ show(c)
}