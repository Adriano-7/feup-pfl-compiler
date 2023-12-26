module Stack (Stack,
              push, pop, top,
              empty, fromList,
              isEmpty) where

data Stack a = Stk [a] deriving Show

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk []) = error "Empty stack"
pop (Stk (_:xs)) = Stk xs

top :: Stack a -> a
top (Stk []) = error "Empty stack"
top (Stk (x:_)) = x

empty :: Stack a
empty = Stk []

fromList :: [a] -> Stack a
fromList = Stk --igual a fromList xs = Stk xs

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

