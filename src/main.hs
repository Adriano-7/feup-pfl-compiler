import Data.Char (isLower, isSpace, isAlpha, isAlphaNum, isDigit)
import Data.List (isInfixOf)

import Stack (Stack, push, pop, top, fromList, isEmpty, newStack,)
import State (State, newState, insert, load, fromList, toStr)
import Control.Monad.Trans.Select (select)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

createEmptyStack :: Stack
createEmptyStack = Stack.newStack

stack2Str :: Stack -> String
stack2Str s | isEmpty s = ""
            | top s == "tt" = "True" ++ middle ++ stack2Str (pop s)
            | top s == "ff" = "False" ++ middle ++ stack2Str (pop s)
            | otherwise = top s ++ middle ++ stack2Str (pop s)
            where middle = if isEmpty (pop s) then "" else ","

createEmptyState :: State
createEmptyState = State.newState

state2Str :: State -> String
state2Str = State.toStr

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  
run (inst:code, stack, state) = case inst of
  Push n -> run (code, push (show n) stack, state)
  Add -> run (code, performAdd stack, state)
  Mult -> run (code, performMult stack, state)
  Sub -> run (code, performSub stack, state)
  Tru -> run (code, push "tt" stack, state)
  Fals -> run (code, push "ff" stack, state)
  Equ -> run (code, performEqu stack, state)
  Le -> run (code, performLe stack, state)
  And -> run (code, performAnd stack, state)
  Neg -> run (code, performNeg stack, state)
  Fetch var -> case load var state of
    "" -> error $ "Run-time error: Variable '" ++ var ++ "' not found"
    val -> run (code, push val stack, state)
  Store var -> run (code, pop stack, insert var (top stack) state)
  Noop -> run (code, stack, state)
  Branch c1 c2 -> case top stack of
    "tt" -> run (c1 ++ code, pop stack, state)
    "ff" -> run (c2 ++ code, pop stack, state)
    _    -> error "Run-time error: Invalid operand for branch"
  Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)

performAdd :: Stack -> Stack
performAdd stack =
  case top stack of
    "tt" -> error "Run-time error"
    "ff" -> error "Run-time error"
    val1 -> case top (pop stack) of
      "tt" -> error "Run-time error"
      "ff" -> error "Run-time error"
      val2 -> push (show (read val1 + read val2)) (pop (pop stack))
    
performMult :: Stack -> Stack
performMult stack =
  case top stack of
    "tt" -> error "Run-time error"
    "ff" -> error "Run-time error"
    val1 -> case top (pop stack) of
      "tt" -> error "Run-time error"
      "ff" -> error "Run-time error"
      val2 -> push (show (read val1 * read val2)) (pop (pop stack))

performSub :: Stack -> Stack
performSub stack =
  case top stack of
    "tt" -> error "Run-time error"
    "ff" -> error "Run-time error"
    val1 -> case top (pop stack) of
      "tt" -> error "Run-time error"
      "ff" -> error "Run-time error"
      val2 -> push (show (read val1 - read val2)) (pop (pop stack))

performEqu :: Stack -> Stack
performEqu stack =
  case top stack of
    val1 -> case top (pop stack) of
      val2 -> if val1 == val2 then push "tt" (pop (pop stack)) else push "ff" (pop (pop stack))

performLe :: Stack -> Stack
performLe stack =
  case top stack of
    "tt" -> error "Run-time error"
    "ff" -> error "Run-time error"
    val1 -> case top (pop stack) of
      "tt" -> error "Run-time error"
      "ff" -> error "Run-time error"
      val2 -> if (read val1 :: Int) <= (read val2 :: Int) then push "tt" (pop (pop stack)) else push "ff" (pop (pop stack))

performAnd :: Stack -> Stack
performAnd stack =
  case top stack of
    "tt" -> case top (pop stack) of
      "tt" -> push "tt" (pop (pop stack))
      "ff" -> push "ff" (pop (pop stack))
      _    -> error "Run-time error: Invalid operand for logical AND"
    "ff" -> case top (pop stack) of
      "tt" -> push "ff" (pop (pop stack))
      "ff" -> push "ff" (pop (pop stack))
      _    -> error "Run-time error: Invalid operand for logical AND"
    _    -> error "Run-time error: Invalid operand for logical AND"     

performNeg :: Stack -> Stack
performNeg stack =
  case top stack of
    "tt" -> push "ff" (pop stack)
    "ff" -> push "tt" (pop stack)
    _    -> error "Run-time error: Invalid operand for logical NOT"

-- Part 2
-- Arithmetic expressions
data Aexp = NumExp Integer         -- Numeric constant
          | VarExp String          -- Variable
          | AddExp Aexp Aexp       -- Addition
          | SubExp Aexp Aexp       -- Subtraction
          | MulExp Aexp Aexp       -- Multiplication
          deriving Show

-- Boolean expressions
data Bexp = TrueExp                -- True constant
          | FalseExp               -- False constant
          | EqArExp Aexp Aexp         -- Equality for arithmetic expressions
          | EqBoolExp Bexp Bexp         -- Equality for boolean expressions
          | LeExp Aexp Aexp         -- Less than or equal
          | NotExp Bexp            -- Logical negation
          | AndExp Bexp Bexp       -- Logical AND
          deriving Show

-- Statements
data Stm = AssignStm String Aexp    -- Assignment
          | SeqStm [Stm]             -- Sequence of statements
          | IfStm Bexp Stm Stm       -- If-then-else statement
          | WhileStm Bexp Stm        -- While loop statement
          deriving Show

type Program = [Stm]

-- Lexer functions
data Token = TokAssign          -- ':='
           | TokSemicolon       -- ';'
           | TokVar String      -- var name
           | TokNumber Integer  -- number
           | TokOpenParen       -- '('
           | TokCloseParen      --')'
           | TokAdd             -- '+'
           | TokSub             -- '-'
           | TokMul             -- '*'
           | TokIf              -- 'if'
           | TokThen            -- 'then'
           | TokElse            -- 'else'
           | TokWhile           -- 'while'
           | TokDo              -- 'do'
           | TokBoolEqu         -- '='
           | TokIntEqu           -- '=='
           | TokLE              -- '<=' 
           | TokNot             -- 'not'
           | TokAnd             -- 'and'
           | TokTrue            -- 'True'
           | TokFalse           -- 'False'    
           deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer input@(c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexIdentifier input
    | isDigit c = lexNumber input
    | c == '(' = TokOpenParen : lexer cs
    | c == ')' = TokCloseParen : lexer cs
    | c == '+' = TokAdd : lexer cs
    | c == '-' = TokSub : lexer cs
    | c == '*' = TokMul : lexer cs
    | c == ':' = lexAssign cs
    | c == '=' = lexEqual cs
    | c == ';' = TokSemicolon : lexer cs
    | c == '<' = lexLessEqual cs
    | otherwise = error $ "Unexpected character: " ++ [c]

lexIdentifier :: String -> [Token]
lexIdentifier input =
    let (ident, rest) = span isAlphaNum input
    in case ident of
      "True" -> TokTrue : lexer rest
      "False" -> TokFalse : lexer rest
      "if" -> TokIf : lexer rest
      "then" -> TokThen : lexer rest
      "else" -> TokElse : lexer rest
      "while" -> TokWhile : lexer rest
      "do" -> TokDo : lexer rest
      "and" -> TokAnd : lexer rest
      "not" -> TokNot : lexer rest
      _ | isLower (head ident) -> TokVar ident : lexer rest
        | otherwise -> error $ "Invalid variable name: " ++ ident   

lexNumber :: String -> [Token]
lexNumber input =
    let (num, rest) = span isDigit input
    in TokNumber (read num) : lexer rest

lexAssign :: String -> [Token]
lexAssign ('=':rest) = TokAssign : lexer rest
lexAssign rest = error $ "Unexpected character after ':': " ++ rest

lexEqual :: String -> [Token]
lexEqual ('=':rest) = TokIntEqu : lexer rest
lexEqual rest = TokBoolEqu : lexer rest

lexLessEqual :: String -> [Token]
lexLessEqual ('=':rest) = TokLE : lexer rest
lexLessEqual rest = error $ "Unexpected character after '<': " ++ rest

lexAnd :: String -> [Token]
lexAnd rest@(c:cs)
    | take 2 rest == "and" = TokAnd : lexer (drop 3 rest)
    | otherwise = error $ "Unexpected character after 'a': " ++ rest

buildData :: [Token] -> Program
buildData tokens = 
  case parseStm tokens of
    Just (stm, []) -> [stm] 
    Just (stm, restTokens) -> stm : buildData restTokens
    _ -> error $ "Unexpected error parsing statement (buildData): " ++ show tokens

selectAexpr :: [Token] -> Aexp
selectAexpr tokens = case parseAexp tokens of
  Just (aexp, []) -> aexp
  _ -> error $ "Unexpected error parsing arithmetic expression: " ++ show tokens

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = case parseSumOrDifOrProdOrIntOrPar tokens of
    Just (aexp, []) -> Just (aexp, [])
    Just (aexp, TokSemicolon:rest) -> Just (aexp, TokSemicolon:rest)
    Just (_, rest) -> error $ "Unparsed tokens (parseA): " ++ show rest
    _ -> error $ "Unexpected error parsing arithmetic expression: " ++ show tokens

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = case parseAndOrMore tokens of
  Just (bexp, []) -> Just (bexp, [])
  Just (bexp, TokThen:rest) -> Just (bexp, TokThen:rest)
  Just (bexp, TokElse:rest) -> Just (bexp, TokElse:rest)
  Just (bexp, TokDo:rest) -> Just (bexp, TokDo:rest)
  Just ( _, rest) -> error $ "Unparsed tokens (parseB): " ++ show rest
  _ -> error $ "Unexpected error parsing boolean expression: " ++ show tokens

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens = case tokens of
  TokVar var : TokAssign : restTokens ->
    case parseAexp restTokens of
      Just (aexp, restTokens1) -> case restTokens1 of
        TokSemicolon : restTokens2 -> Just (AssignStm var aexp, restTokens2)
        _ -> error "Missing semicolon after assignment"
      Nothing -> error "Failed to parse arithmetic expression"

  TokIf : restTokens1 ->
    case parseBexp restTokens1 of
      Just (bexp, restTokens2) -> case restTokens2 of
        TokThen : restTokens3 ->
          case parseStm restTokens3 of
            Just (stm1, TokElse : restTokens4) ->
              case parseStm restTokens4 of
                Just (stm2, TokSemicolon : restTokens5) ->
                  Just (IfStm bexp stm1 stm2, restTokens5)
                _ -> error "Missing semicolon after 'else' statement"
            _ -> error "Missing 'else' after 'then' statement"
        _ -> error "Missing 'then' after 'if' statement"
      Nothing -> error "Failed to parse boolean expression"

  TokWhile : restTokens1 ->
    case parseBexp restTokens1 of
      Just (bexp, restTokens2) -> case restTokens2 of
        TokDo : restTokens3 ->
          case parseStm restTokens3 of
            Just (stm, TokSemicolon : restTokens4) ->
              Just (WhileStm bexp stm, restTokens4)
            _ -> error "Missing semicolon after 'do' statement"

        _ -> error "Missing 'do' after 'while' statement"
      Nothing -> error "Failed to parse boolean expression"

  TokOpenParen : restTokens1 ->
    case parseSeqStm restTokens1 of
      Just (stmList, TokSemicolon : restTokens2) ->
        Just (SeqStm stmList, restTokens2)
      _ -> error "Missing semicolon after sequence of statements"
  
  _ -> error $ "Unexpected error parsing statement: " ++ show tokens

parseSeqStm :: [Token] -> Maybe ([Stm], [Token])
parseSeqStm tokens =
  case parseStm tokens of
    Just (stm, restTokens1) -> case restTokens1 of
      TokSemicolon : restTokens2 ->
        case parseSeqStm restTokens2 of
          Just (stmList, restTokens3) -> Just (stm : stmList, restTokens3)
          Nothing -> Nothing
      _ -> Just ([stm], restTokens1)
    Nothing -> Just ([], tokens)

--parserA auxiliary functions
parseSumOrDifOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrDifOrProdOrIntOrPar tokens =
  case parseProdOrIntOrPar tokens of
    Just (expr1, restTokens1) ->
      parseSumOrDifOrProdOrIntOrParAux expr1 restTokens1
    _ -> Nothing

parseSumOrDifOrProdOrIntOrParAux :: Aexp -> [Token] -> Maybe (Aexp, [Token])
parseSumOrDifOrProdOrIntOrParAux expr1 tokens =
  case tokens of
    TokAdd : restTokens1 -> do
      (expr2, restTokens2) <- parseProdOrIntOrPar restTokens1
      parseSumOrDifOrProdOrIntOrParAux (AddExp expr1 expr2) restTokens2
    TokSub : restTokens1 -> do
      (expr2, restTokens2) <- parseProdOrIntOrPar restTokens1
      parseSumOrDifOrProdOrIntOrParAux (SubExp expr1 expr2) restTokens2
    _ -> Just (expr1, tokens)


parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (TokNumber n : restTokens)
  = Just (NumExp n, restTokens)

parseIntOrParenExpr (TokVar var : restTokens)
  = Just (VarExp var, restTokens)

parseIntOrParenExpr (TokOpenParen : restTokens1)
  = case parseSumOrDifOrProdOrIntOrPar restTokens1 of
    Just (expr, TokCloseParen : restTokens2) ->
      Just (expr, restTokens2)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens
  = case parseIntOrParenExpr tokens of
    Just (expr1, TokMul : restTokens1) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (MulExp expr1 expr2, restTokens2)
        _ -> Nothing
    result -> result

--parserB auxiliary functions
isAritmeticToken :: Token -> Bool
isAritmeticToken (TokNumber _) = True
isAritmeticToken (TokVar _) = True
isAritmeticToken tok = tok `elem` [ TokAdd, TokSub, TokMul]

pickAritmeticTokens :: [Token] -> ([Token], [Token])
pickAritmeticTokens tokens = pickAritmeticTokensAux tokens 0 []


pickAritmeticTokensAux :: [Token] -> Int -> [Token] -> ([Token], [Token])
pickAritmeticTokensAux [] 0 acc = (reverse acc, [])
pickAritmeticTokensAux [] n acc = error $ "Unbalanced parentheses " ++ show (reverse acc) ++ " " ++ show n
pickAritmeticTokensAux (TokOpenParen : t) balance acc = pickAritmeticTokensAux t (balance + 1) (TokOpenParen : acc)
pickAritmeticTokensAux (TokCloseParen : t) 0 acc = (reverse acc, TokCloseParen : t)
pickAritmeticTokensAux (TokCloseParen : t) balance acc = pickAritmeticTokensAux t (balance - 1) (TokCloseParen : acc)
pickAritmeticTokensAux (h : t) balance acc
    | isAritmeticToken h = pickAritmeticTokensAux t balance (h : acc)
    | otherwise = (reverse acc, h : t)

parseConstOrParen :: [Token] -> Maybe (Bexp, [Token])
parseConstOrParen (TokTrue : tokens) = Just (TrueExp, tokens)
parseConstOrParen (TokFalse : tokens) = Just (FalseExp, tokens)
parseConstOrParen (TokOpenParen : tokens) = case parseAndOrMore tokens of
  Just (bexp, TokCloseParen : restTokens) -> Just (bexp, restTokens)
  _ -> error "Missing closing parenthesis"
parseConstOrParen tokens = error $ "Unexpected tokens: " ++ show tokens

parseLE :: [Token] -> Maybe (Bexp, [Token])
parseLE tokens = 
  case pickAritmeticTokens tokens of
    ([], _) -> parseConstOrParen tokens
    (aTokens, TokLE:rest) -> case pickAritmeticTokens rest of
      ([], _) -> parseConstOrParen tokens
      (aTokens2, rest2) -> Just (LeExp (selectAexpr aTokens) (selectAexpr aTokens2), rest2)
    _ -> parseConstOrParen tokens

parseIntEqOrMore :: [Token] -> Maybe (Bexp, [Token])
parseIntEqOrMore tokens =
  case pickAritmeticTokens tokens of
    ([], _) -> parseLE tokens
    (aTokens, TokIntEqu:rest) -> case pickAritmeticTokens rest of
      ([], _) -> parseLE tokens
      (aTokens2, rest2) -> Just (EqArExp (selectAexpr aTokens) (selectAexpr aTokens2), rest2)
    _ -> parseLE tokens

parseNotOrMore :: [Token] -> Maybe (Bexp, [Token])
parseNotOrMore (TokNot : tokens) = case parseIntEqOrMore tokens of
  Just (bexp, restTokens) -> Just (NotExp bexp, restTokens)
parseNotOrMore tokens = parseIntEqOrMore tokens

parseBoolEqOrMore :: [Token] -> Maybe (Bexp, [Token])
parseBoolEqOrMore tokens = case parseNotOrMore tokens of
  Just (bexp, TokBoolEqu : restTokens) -> case parseBoolEqOrMore restTokens of
    Just (bexp2, restTokens2) -> Just (EqBoolExp bexp bexp2, restTokens2)
  result -> result

parseAndOrMore :: [Token] -> Maybe (Bexp, [Token])
parseAndOrMore (TokOpenParen:rest) = parseConstOrParen (TokOpenParen:rest)
parseAndOrMore tokens = case parseBoolEqOrMore tokens of
  Just (bexp, TokAnd : restTokens) -> case parseAndOrMore restTokens of
    Just (bexp2, restTokens2) -> Just (AndExp bexp bexp2, restTokens2)
  result -> result

-- Compiler functions
compA :: Aexp -> Code
compA (NumExp n)     = [Push n]
compA (VarExp var)   = [Fetch var]
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MulExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB TrueExp         = [Tru]
compB FalseExp        = [Fals]
compB (EqArExp a1 a2)   = compA a2 ++ compA a1 ++ [Equ]
compB (EqBoolExp b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LeExp a1 a2)   = compA a2 ++ compA a1 ++ [Le]
compB (NotExp b)      = compB b ++ [Neg]
compB (AndExp b1 b2)  = compB b2 ++ compB b1 ++ [And]

compile :: Program -> Code
compile []                 = []
compile (AssignStm var a : rest) = compA a ++ [Store var] ++ compile rest
compile (SeqStm stms : rest)      = compile stms ++ compile rest
compile (IfStm b s1 s2 : rest) = compB b ++ [Branch (compile [s1]) (compile [s2])] ++ compile rest
compile (WhileStm b s : rest) = Loop (compB b) (compile [s]) : compile rest

parse :: String -> Program
parse = buildData . lexer

testParseStm :: IO ()
testParseStm = do
    let string = "x := 5; y := 2;"
    print string
    let tokens1 = lexer string
    print tokens1
    let result1 = buildData tokens1
    print result1

    let compiledCode1 = compile result1
    print compiledCode1

    print (testAssembler compiledCode1)

{--
testParseAexp :: IO ()
testParseAexp = do
    let string = "2"
    print string
    let tokens1 = lexer string
    print tokens1
    let result1 = parseAexp tokens1 
    print result1

    let compiledCode1 = compA result1
    print compiledCode1

    print (testAssembler compiledCode1)

testParseBexp :: IO ()
testParseBexp = do
    let string = "(True = 3 == 4)"
    print string
    print ""
    let tokens1 = lexer string
    print tokens1
    print ""
    let result1 = parseBexp tokens1 
    print result1
    print ""

    let compiledCode1 =  compB result1
    print compiledCode1
    print ""

    print (testAssembler compiledCode1)
--}
{--
buildData :: [Token] -> Program
buildData [] = []
buildData tokens = case parseStm tokens of
    (stm, restTokens) -> stm : buildData restTokens
--}

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- testAssembler [Push 1, Push 2, Tru, Branch [Sub, Tru] [Add, Fals], Store "x"] == ("1","x=True")
-- testAssembler [Push 1, Push 2, Fals, Branch [Sub, Tru] [Add, Fals], Store "x"] == ("3","x=False")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"], Tru] == ("True","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Example lexer test function
testLexer :: String -> IO ()
testLexer input = do
    let tokens = lexer input
    putStrLn $ "Input String: " ++ show input
    putStrLn $ "Tokens: " ++ show tokens

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

{-- Examples to test the compiler without the parser
main :: IO ()
main = do
  -- testLexer "x := 5; x := x - 1;"
  {--
    --Example 1 "x := 5; x := x - 1;" == ("","x=4")
    let expression = [AssignStm "x" (NumExp 5), AssignStm "x" (SubExp (VarExp "x") (NumExp 1))]
  --}
  {--
    -- Example 2 "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    let expression = [IfStm (AndExp (NotExp TrueExp) (EqBoolExp (LeExp (NumExp 2) (NumExp 5)) (EqArExp (NumExp 3) (NumExp 4)))) (AssignStm "x" (NumExp 1)) (AssignStm "y" (NumExp 2))]
  --}
  {--
    -- Example 3 "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
    let expression = [AssignStm "x" (NumExp 42), IfStm (LeExp (VarExp "x") (NumExp 43)) (AssignStm "x" (NumExp 1)) (SeqStm [AssignStm "x" (NumExp 33), AssignStm "x" (AddExp (VarExp "x") (NumExp 1))])]
  --}
  {--
    -- Example 4 "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    let expression = [AssignStm "x" (NumExp 42), IfStm (LeExp (VarExp "x") (NumExp 43)) (AssignStm "x" (NumExp 1)) (AssignStm "x" (NumExp 33)), AssignStm "x" (AddExp (VarExp "x") (NumExp 1))]
  --}
  {--
    -- Example 5 "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    let expression = [AssignStm "x" (NumExp 42), IfStm (LeExp (VarExp "x") (NumExp 43)) (AssignStm "x" (NumExp 1)) (AssignStm "x" (NumExp 33)), AssignStm "x" (AddExp (VarExp "x") (NumExp 1)), AssignStm "z" (AddExp (VarExp "x") (VarExp "x"))]
  --}
  {--
    -- Example 6 "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    let expression = [AssignStm "x" (NumExp 2), AssignStm "y" (MulExp (SubExp (VarExp "x") (NumExp 3)) (AddExp (NumExp 4) (MulExp (NumExp 2) (NumExp 3)))), AssignStm "z" (AddExp (VarExp "x") (MulExp (VarExp "x") (NumExp 2)))]
  --}
    --Example 7 "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
    let expression = [AssignStm "i" (NumExp 10), AssignStm "fact" (NumExp 1), WhileStm (NotExp (EqArExp (VarExp "i") (NumExp 1))) (SeqStm [AssignStm "fact" (MulExp (VarExp "fact") (VarExp "i")), AssignStm "i" (SubExp (VarExp "i") (NumExp 1))])]
    let compiledCode = compile expression

    print compiledCode
    print (testAssembler compiledCode)
--}
