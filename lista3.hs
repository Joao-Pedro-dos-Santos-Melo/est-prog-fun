

converte :: Char -> (Char, Char, Int)
converte c
    |c >= 'a' && c <= 'z' = (c, toEnum(fromEnum c - 32), fromEnum c)
    |c >= 'A' && c <= 'Z' = (toEnum(fromEnum c + 32), c, fromEnum c)
    |otherwise = (c, c, fromEnum c)

retornaPri  :: (a, b, c) -> a
retornaPri (x, _, _) = x

retornaSeg  :: (a, b, c) -> b
retornaSeg (_, x, _) = x

retornaTri  :: (a, b, c) -> c
retornaTri (_, _, x) = x

pessoa :: Int -> (String, Int, Char)
pessoa rg
    |rg == 1 = ("Joao", 12, 'm')
    |rg == 2 = ("Lucas", 25, 'm')
    |rg == 3 = ("Cobaia", 99, 'f')
    |rg == 4 = ("maria", 24, 'f')
    |rg == 5 = ("Felipe", 18, 'm')
    |rg == 6 = ("Eduardo", 24, 'f')
    |otherwise = ("Erro", 9999, 'x')

menorIdade :: Int -> String
menorIdade i = retornaPri (pessoa (achaMI i (i -1)))

achaMI :: Int -> Int -> Int
achaMI 0 y = y
achaMI x 0 = x
achaMI x y
    |retornaSeg (pessoa x) < retornaSeg (pessoa y) = achaMI x (y-1)
    |otherwise = achaMI y (y - 1)

mediaIdade :: Int -> Float
mediaIdade q = fromIntegral (somaIdade q) / fromIntegral q

somaIdade :: Int -> Int
somaIdade 0 = 0
somaIdade i = retornaSeg (pessoa i) + somaIdade (i - 1)

namM :: Int
namM = numMas 1

numMas :: Int -> Int
numMas x
    |retornaTri (pessoa x) == 'm' = numMas (x + 1) + 1
    |retornaTri (pessoa x) == 'f' = numMas (x + 1)
    |otherwise = 0

numMI :: Int
numMI = numMIC 1

numMIC :: Int -> Int
numMIC x
    |retornaSeg (pessoa x) < 18 = numMIC (x + 1)
    |retornaSeg (pessoa x) == 9999 = 0
    |otherwise = numMIC (x + 1) + 1

ordena :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordena x y z w = transListaTupla (inseri x (inseri y (inseri z (inseri w []))))

transListaTupla :: [Int] -> (Int, Int, Int, Int)
transListaTupla (x : y : z : w : _) = (x, y, z, w)

inseri :: Int -> [Int] -> [Int]
inseri x [] = [x]
inseri x (a : b)
    |x <= a = x : a : b
    |otherwise = a : inseri x b 
