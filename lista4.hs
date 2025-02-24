
ex1 :: Int
ex1 = sum [x^2 | x <- [1..100]]

ex2 :: Int -> a -> [a]
ex2 n x = [x | _ <- [1.. n]]

ex3 :: Int -> [(Int, Int, Int)]
ex3 n = [(x, y, z)| x <-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2]

ex4 :: Int -> [Int]
ex4 n = [x | x <- [1..n], sum (factors x) == x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], n `mod` x == 0]

tes :: [(Int, Int)]
tes = [(x , y) | x <- [1..5], y <- [2, 4..8]]

