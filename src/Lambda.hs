type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

------------------------- Assignment 1

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (iterate (Apply (Variable "f")) (Variable "x") !! i))

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2

variables :: [Var]
variables = [[c] | c <- ['a'..'z']] ++ [c : show i | i <- [1..], c <- ['a'..'z']]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs ys = filter (`notElem` ys) xs

fresh :: [Var] -> Var
fresh xs = head (filterVariables variables xs)

used :: Term -> [Var]
used (Variable z) = [z]
used (Lambda z m) = merge [z] (used m)
used (Apply  m n) = merge (used m) (used n)

------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z
rename x y (Lambda z m)
    | z == x    = Lambda z m
    | otherwise = Lambda z (rename x y m)
rename x y (Apply  m n) = Apply (rename x y m) (rename x y n)

substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | y == x    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | y == x    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
      where
        z = fresh (merge (merge (used m) (used n)) [x])
substitute x n (Apply m1 m2) = Apply (substitute x n m1) (substitute x n m2)

------------------------- Assignment 4

beta :: Term -> [Term]
beta (Apply (Lambda z m) n) = substitute z n m : betaM ++ betaN
  where
    betaM = [Apply (Lambda z i) n | i <- beta m]
    betaN = [Apply (Lambda z m) i | i <- beta n]
beta (Variable z) = []
beta (Lambda z m) = [Lambda z i | i <- beta m]
beta (Apply  m n) = [Apply i n | i <- beta m] ++ [Apply m i | i <- beta n]

normalize :: Term -> [Term]
normalize m
    | null (beta m) = [m]
    | otherwise = m : normalize (head (beta m))

normal :: Term -> Term
normal m = last (normalize m)

------------------------- 

a_beta :: Term -> [Term]
a_beta (Apply (Lambda z m) n) = a_betaM ++ a_betaN ++ [substitute z n m]
  where
    a_betaM = [Apply (Lambda z i) n | i <- a_beta m]
    a_betaN = [Apply (Lambda z m) i | i <- a_beta n]
a_beta (Variable z) = []
a_beta (Lambda z m) = [Lambda z i | i <- a_beta m]
a_beta (Apply  m n) = [Apply i n | i <- a_beta m] ++ [Apply m i | i <- a_beta n]

a_normalize :: Term -> [Term]
a_normalize m
    | null (a_beta m) = [m]
    | otherwise = m : a_normalize (head (a_beta m))

a_normal :: Term -> Term
a_normal m = last (a_normalize m)

-------------------------

example1 :: Term
example1 = Apply (numeral 2) (numeral 2)

example2 :: Term
example2 = Apply (Lambda "x" (Variable "y")) example1
