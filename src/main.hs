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

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

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