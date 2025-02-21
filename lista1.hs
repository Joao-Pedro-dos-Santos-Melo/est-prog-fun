import Data.Char

e1a :: Float -> Float
e1a x
    |x < 0 = 2 / x
    |otherwise = (x + 4)/(x + 2)

e1b :: Float -> Float -> Float
e1b x y
    |x < y = x - y
    |otherwise = x + y

e1c :: Float -> Float -> Float -> Float
e1c x y z
    |x + y > z = x + y + z
    |x + y < z = x - y - z
    |otherwise = 0

fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x - 1)

multiplicacao :: Int -> Int -> Int
multiplicacao _ 0 = 0
multiplicacao x y = x + multiplicacao x (y - 1)

invertInt :: Int -> Int
invertInt n = read (reverse (show (abs n))) * signum n

fourPower :: Int -> Int
fourPower x = x ^ 4

ex6 :: Int -> Float
ex6 0 = sqrt 6
ex6 x = sqrt (6 + ex6 (x - 1))

ex7 :: Int -> Int -> Int
ex7 m n
    |m < n = error "m deve ser maior ou igual a n"
    |otherwise = fat m `div` (fat n * fat (m - n))

ex8 :: Int -> Int -> Int
ex8 m n
    | n == 0 = m
    |otherwise = ex8 n (mod m n)

ex9 :: Int -> Int -> Int -> Int
ex9 n x y
    |x > y = 0
    |mod x n == 0 = 1 + ex9 n (x+1) y
    |otherwise = ex9 n (x+1) y

ex10 :: Int -> Int
ex10 x = mod x 10

anyDigit :: Int -> Int -> Int
anyDigit x y = ex11 x (show y)

ex11 :: Int -> String -> Int
ex11 0 (a : b) = fromEnum a - fromEnum '0'
ex11 x (a : b)
    |x < length (a : b) = ex11 (x - 1) b
    |otherwise = (-1)

allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = ((m /= n) && (n /= p)) && (m /= p)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    |(x == y) && (y == z) = 3
    |(x == y) || (y == z) || (x == z) = 2
    |otherwise = 0

ex17:: Char -> Char
ex17 c
    | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
    | otherwise = c

ex18 :: Char -> Int
ex18 c
    |isDigit c = ord c - ord '0'
    |otherwise = -1

ex19 :: String -> Int -> String
ex19 _ 0 = ""
ex19 s x = s ++ ex19 s (x -1)

ex20 :: String -> Int -> String
ex20 s n
    |n <= length s = s
    |otherwise = ">" ++ ex20 s (n - 1)


(&-) :: Num a => a -> a -> a
x &- y = x - 2 * y

ex22 :: [Int] -> [Int]
ex22 [] = []
ex22 (a : b) = ex22 b ++ [a]

ex23 :: [Int] -> ([Int], [Int])
ex23 v = separa v ([],[])

separa :: [Int] -> ([Int], [Int]) -> ([Int],[Int])
separa [] t = t
separa (a : c) (i, p)
    |mod a 2 == 0 = separa c (i, (p ++ [a]))
    |otherwise = separa c ((i ++ [a]), p)

ex24 :: [Int] -> String
ex24 [] = ""
ex24 (n : c) = chr (n + 64) : ex24 c

ex25 :: String -> Char -> Int
ex25 [] _ = 0
ex25 (i : c) l
    |i == l = ex25 c l + 1
    |otherwise = ex25 c l

ex27 :: [Int] -> [Int]
ex27 v = reverse(puri v [])

puri :: [Int] -> [Int] -> [Int]
puri [] v = v
puri (a : c) [] = puri c [a]
puri (a : c) (b : d)
    |a == b = puri c (b : d)
    |otherwise = puri c (a : b : d)

ex28 :: [Int] -> [Int]
ex28 [] = []
ex28 (n : c) = ml n n ++ ex28 c

ml :: Int -> Int-> [Int]
ml 0 _ = []  -- Caso base: se n for 0, retorna lista vazia
ml q n = n : ml (q - 1) n

