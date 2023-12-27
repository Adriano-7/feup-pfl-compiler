module Stack (Stack,
              push, pop, top,
              empty, fromList,
              isEmpty) where

newtype Stack = Stk [String] deriving Show

push :: String -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk []) = error "Empty stack"
pop (Stk (_:xs)) = Stk xs

top :: Stack -> String
top (Stk []) = error "Empty stack"
top (Stk (x:_)) = x

empty :: Stack
empty = Stk []

fromList :: [String] -> Stack
fromList = Stk --igual a fromList xs = Stk xs

isEmpty :: Stack -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

