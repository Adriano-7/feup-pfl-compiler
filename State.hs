module State (State,
              newState, insert, load,
              fromList, toList, toStri) where

data State = Empty 
            | Node String String State State

newState :: State
newState = Empty

fromList :: [(String, String)] -> State
fromList [] = newState
fromList ((var,key):xs) = insert var key (fromList xs)

insert :: String -> String -> State -> State
insert x v Empty = Node x v Empty Empty
insert x v (Node y u l r) | x == y = Node x v l r
                          | x > y = Node y u l (insert x v r)
                          | x < y = Node y u (insert x v l) r

load :: String -> State -> String
load x Empty = "" -- não encontrou
load x (Node y v l r) | x == y =  v -- encontrou
                      | x > y  = load x r
                      | x < y  = load x l

listToStr :: [(String, String)] -> String
listToStr [] = ""
listToStr ((var,key):[]) = var ++ "=" ++ key
listToStr ((var,key):xs) = var ++ "=" ++ key ++ "," ++ listToStr xs

toList :: State -> [(String, String)]
toList Empty = []
toList (Node x v l r) = toList l ++ [(x,v)] ++ toList r

toStr :: State -> String
toStr s = listToStr (toList s)
