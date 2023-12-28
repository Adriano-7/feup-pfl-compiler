import Stack (Stack, push, pop, top, fromList, isEmpty, newStack,)
import State (State, newState, insert, load, fromList, toStr)

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

compile :: [Stm] -> Code
compile []                 = []
compile (AssignStm var a : rest) = compA a ++ [Store var] ++ compile rest
compile (SeqStm stms : rest)      = compile stms ++ compile rest
compile (IfStm b s1 s2 : rest) = compB b ++ [Branch (compile [s1]) (compile [s2])] ++ compile rest
compile (WhileStm b s : rest) = Loop (compB b) (compile [s]) : compile rest

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")


-- Examples to test the compiler without the parser
main :: IO ()
main = do
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
