# NeoMass
Custom Domain Language with interpreter written in Haskell

NeoMass is a high level, dynamically typed programming languages featuring a small syntax set and support for lists. Closely modelled on JavaScript due to its ease of use and knowledge, NMS aims to remove redundant syntax and constraints such as different tokens for conditionals (not, negate , concatenate and add) and merges them into one, leaving the interpreter to infer intention. As a result it is quite flexible and source text is small for most use cases specified in the domain specification. While the language is suitable to solve the problems defined in the specifications, there still remains work to be done to implement type checking (for consistency), better error handling and edge cases.

## Main Language Features
### Support for Lists
NMS supports lists as a data structure and is treated as a first-class object. Lists can be passed around as referenced values and be stored in variables as a result. Objects within the lists are automatically converted to expressions which allows list manipulation using NeoMass expressions.

### Descriptive Syntax
NMS closely follows the C syntax as a rule of familiarity and convenience. Many of the world’s most used languages follow the C Syntax (Java, C,C++ etc…) and on a personal level, I found the syntax easy to understand and abstract problems into code when first learning how to program. As a result, the language was designed from the very beginning with this in mind. Where the syntax differs is in the type declarations. NeoMass has two declarative types (var and int). This is in contrast to the large set of types included in Java however I believe that this is unnecessary for this problem domain as it is exclusively using streams of integers and consequently any other use of types is mostly for convenience. Furthermore, this enables a fairly simple mechanism for type checking and

### Standard Library
NMS comes included with typical functions developers may need to solve a problem while reducing boilerplate code. Such functions are: print, isEmpty, length, getStream and getStreamElement 


## Syntax

    !! A NeoMass Program
    print “hello world”;

The goal for developing the syntax for NeoMass was: Compact and Useful. I believe that by achieving these two objectives, we also achieve precision. By achieving precision, the user experience of the programmer is improved since their intentions can be reached with less text and thus is more readable. 

## Data types

### Booleans

      true; !! Not False
      false; !! Not *not* False;

### Numbers
In NeoMass, the only kind of number supported is the integer. Quite simply, all input streams are integers so it is reasonable to assume that any transformations will only need integers. 
It was noted during the design phase the addition of floating point for calculations and the interpreter can easily be extended to include this.

      1234; !! Allowed number
      -12;  !! Allowed number
      12.3; !! Not allowed (will throw a lexical error)

### Strings - Like most languages, a string literal is enclosed in double quotes

      "Hi, I am a String!" "Valid String
      "Please to meet you String." !! Accepted String
      "" !! The empty string
      "0" !! A String not a number

### Null
Null was decided to be included in our program to allow the developer to distinguish between an unintialised value, an empty value (in the case of string) or a nil value (Integers). There is no real use for this data type apart


## Expressions

### Arithmetic
Arithmetic expressions are evaluated with infix Operators. The negation operator can also be used in prefix form to negate a number (or invert a Boolean)

        1+1; !! Addition
        10-0; !! Subtraction
        100000/32; !! Division
        898*9; !! Multiplication

### Comparison

        ExprBool < ExprBool; !! Less Than
        ExprBool > ExprBool; !! More Than
        ExprBool <= ExprBool; !! Less Than or Equal
        ExprBool >= ExprBool; !! More Than or Equal

### Equality
We can test two values of any kind for equality or inequality.

        9 == 9 !! True
        "HelloWorld" != "NeoMass is a great language deserving of a high grade" !! False (not the high grade bit, please)


### Logical Operators
Not operator returns a true if its operand is false and vice versa. This was included in line with the familiarity objective when designing the syntax. Note that the operator '-' is also a logical operator

        !true;          !! False
        !false          !! True
        -(9<10)         !! False
        -("Hi" != "Hi") !! True

        The logical operator for "and" is '&&'.
        true && true;   !! True
        true && false;  !! False

        Finally for "or" we have '||'
        true || false;  !! True
        false || false; !! False


## Statements
Print statement evaluates the expression declared and outputs its result onto console for the user.

        print "NeoMass is a great language deserving of a high grade"; !! NeoMass is a great language deserving of a high grade

An Expression Statement similarly follows that it consists of a declared expression and a semi colon to denote the statement

        2+4; 
        "Hi I am an expression";
        (true && False);

Statements can be blocked into a sequences of statements to signal grouping using the brackets lexeme '{''}'

        {
            print "NeoMass is a great language";
            print "deserving of a high grade";
        }

## Variables
Variables are declared using var and int statements. variables declared without assignment are initialised with null and zero respectively.

        var variable = "myValue";
        var iAmNull; 

        int variable = 0;
        int iAmZero;

Variables can then be accessed and assign using its identifier;

        var string = "NeoMass is a great language deserving of a high grade";
        print string; !! NeoMass is a great language deserving of a high grade

        var string = "For reiteration, NeoMass is a great language deserving of a high grade";
        print string; !! For reiteration, NeoMass is a great language deserving of a high grade

## Control Flow

The If Else Statement evaluates either one of two statement sequences based on the evaluation of the condition.

        if("NeoMass is a great language deserving of a high grade" == True) {
            print "Yes it is";
        } else {
            print "No it is not";
        };

        !! Yes it is.

Alternatively, the while statement could evaluate a statement sequence ‘while’ some condition remains true

        int count = 0;
        while (count < 10) {
            print count;;
            count++;
        };


## Scope and Lexical Rules
### Precedence and Associativity

Precedence and Associativity follows the C structure exactly. For brevity you can find this structure in the appendix but, this decision was made in accordance with the familiarity objective I set out when designing the language and in addition, makes the most sense in my view for evaluating expressions. I briefly looked at other languages for precedence and associativity such as Haskell and Python, both fairly concise and expressive languages however I believe that the value added by choosing this over C was limited since we are working with fairly simple expressions and data structures and C's structure is more than sufficient.

Assignments are right associative
a = b = c is equivalent to a = (b = c)

Ambiguous operations are left associative
a - b - c is equivalent to (a - b) - c

The full precedence rules are included in the appendix

The language thus remains simple and intuitive. NMS removes the need for redundant parentheses since the precedence is assumed to follow the user's understanding of precedence and thus trusts that the interpreter can infer correctly. Where this is incorrect, the user can specify the grouping themselves.

## Execution Model
The execution of the program models that of a CEK Machine. 
Control: Set of all statements in the source text
Environment: First-class function that looks up variables in a list corresponding to the local evaluation context of the expression. Stored as a Haskell Data Type: [(String, Type, Expr)]

Continuations: Return of an IO ([Environment]) Monad which is a return of the environment in the program with the latest as its head. This includes the remainder of the evaluated inputs.

The execution of the program is done sequentially according to the hierarchy of grammar structure
A program consists of a list of statementSequences
A statement Sequence is a list of Statements
A statement can be one of 6 Statements: Return Statement, Condition Statement, Expression Statement, Assign ID Statement, Print Statement, While Statement

Thus the execution of these statements by pattern matching against their corresponding Abstract Data Types in order to create an abstract syntax tree for evaluation.

evaluateExpr is where the majority of the runtime states occur and its output is (Expr, Input) where the expression is the evaluated outcome of the input expression and the remainder of input processed
evaluateExpr then pattern matches against the types of expressions in the grammar again as an ADT. This evaluation then calls on its respective function to calculate the expression before returning the evaluated expression and input

For example:

        print 2 + 2;
        1. Evaluate Program
        evaluateProgram [Program [PrintStmt 
                                        (Expr 
                                            (Add (IntegerLiteral 2 ) (IntegerLiteral 2))]]

        2. Evaluate Statement Sequence
        evaluateStatementSequence [PrintStmt (Expr (Add (IntegerLiteral 2 ) (IntegerLiteral 2)))]

        3. Evaluate PrintStmt (Expr (Add (IntegerLiteral 2) (IntegerLiteral 2)))

        4. Evaluate Expression 
        evaluateExpr (Add (IntegerLiteral 2) (IntegerLiteral 2))

        5. Add 2 + 2  
        evaluateAddExpr (Add (IntegerLiteral 2) (IntegerLiteral 2)) !! Returns IntegerLiteral 4

        6. Evaluate Expression (IntegerLiteral 4)
        evaluateExpr (IntegerLiteral 4) !! Returns IntegerLiteral 4

        7. Evaluate Print Statement (IntegerLiteral 4)
        evaluatePrintStatement (PrintStmt (IntegerLiteral 4)) !! Outputs 4 to console

        8. Evaluate Statement Sequence
        evaluateStatementSequence [Program[]] !! []

        9. evaluateProgram [] !! []
        10. Return environment, input -> returns [], input (Since there was no variables assigned or declared)

It should be noted that execution model is the same for variables in which the corresponding variables is first looked up in the environment and returns the expression if matching the name and type or an error "No bindings" otherwise.

        ten = 10;
        print ten;

        1. Evaluate Program
        evaluateProgram [AssignStmt (AssignID (VarReference "ten") (IntegerLiteral 10)),PrintStmt (PrintExpr (IntegerLiteral 10))]

        2. Evaluate Statement Sequence
        evaluateStatementSequence [AssignStmt (AssignID (VarReference "ten") (IntegerLiteral 10)),PrintStmt (PrintExpr (IntegerLiteral 10))]

        3. Assign Variable with value
        evaluateStatement (AssignStmt (AssignID (VarReference "ten") (IntegerLiteral 10))) 
            3.1 evaluateExpression (IntegerLiteral 10)
                evaluateExpr (IntegerLiteral 10) !! IntegerLiteral 10
            3.2 assign variable to expression
                assignVariable (VarReference "ten") (IntegerLiteral 10) environment
                    3.2.1 Looks for variable already existing in which case update if true
                    3.2.2 Otherwise add to list of environments the new environment1 (VarReference "ten") (VarType var) IntegerLiteral 10 environment1
                    3.2.3 Return the new list of environments

        4. Evaluate PrintStmt (PrintStmt (Var (VarReference "ten"))

        5. Evaluate Expression 
        evaluateExpr (Var(VarReference "ten")) 
            5.1 lookup Variable in Environments
            5.2 return expression and input  -> ((IntegerLiteral 10),input)

        8. Evaluate Print Statement (IntegerLiteral 10)
        evaluatePrintStatement (PrintStmt (IntegerLiteral 10)) !! Outputs 10 to console

        9. Evaluate Statement Sequence
        evaluateStatementSequence [Program[]] !![]

        10. evaluateProgram [] !![]
        11. Return environment, input -> returns ([[("ten", var,IntegerLiteral 10)]], input)

TLDR: 
        evaluateProgram
                        -> 
                            evaluateStatementSequence
                                                -> evaluateStatement
                                                                    -> evaluateExpr
                                                                                    -> Calculates Expression or  Looks it up in environments
                                                                                <-
                                                                    evaluateExpr
                                                                <-
                                                evaluateStatement
                                            <-
                            evaluateStatementSequence
                        <-
        evaluateProgram                                          

Additional Features

Syntactic Sugar
        '::' getStreamElement with choice of (int) for index of stream or singular element inside an array or (int,int) to specify which stream and index for that stream

Example
        sequence = [[]];
        seq = sequences.getStreamElement(0); !! Returns the first index stream

        [[]]::(0); Is the same

        '[[]]' Shorthand for input streams. The user can create a stream by adding a . to end of this Shorthand

'+' can also work to concatenate two arrays
        [10,9,8,7,6] + [5] = [10,9,8,7,6,5]

Example
        sequences = inputStreams;
...
        sequences = [[]]; 

':'shorthand for .getStream similar to getStream index. allows the user to specify index of specific stream

        sequences:(10); !! 10th element from sequences array (fails if sequences not declared as a singular array)

inputStreams also automatically maps all inputs in the stream to IntegerLiteral to then enable further processing by the source code.

        variable ++; !!Increment variable by 1. Handy for index count;

Error Messages
Error messages are displayed to the user by the function that failed to easily details where it failed in the computation. Each error message is of the form (function Name: message: expression (if applicable index))
        "getSingularStream: Could not get the element in list: " ++ show e ++ show i"
