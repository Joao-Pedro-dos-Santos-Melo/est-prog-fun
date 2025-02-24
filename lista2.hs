

type Isbn = Int
type Volumes = Int
type Titulo = String
type Matricula = String
type Reserva = Bool
type Acervo = [(Isbn, Titulo, Reserva, Volumes)]
type Emprestimo = [(Matricula, Isbn)]

acervo ::[(Isbn, Titulo, Reserva, Volumes)]
acervo = 
    [ (9783161484100, "Programação em Haskell", True, 3)
    , (9780123456789, "Estruturas de Dados", False, 5)
    , (9781123456789, "Estruturas de Dados 2", True, 5)
    , (9781234567897, "Paradigmas de Linguagens", True, 2)
    ]


emprestimo :: [(Matricula, Isbn)]
emprestimo =
    [ ("2023001", 9783161484100)  -- Aluno 2023001 pegou "Programação em Haskell"
    , ("2023002", 9781123456789)  -- Aluno 2023002 pegou "Estruturas de Dados 2"
    , ("2023003", 9781234567897)  -- Aluno 2023003 pegou "Paradigmas de Linguagens"
    ]


func_1 :: Isbn -> Acervo -> Bool
func_1 _ [] = False
func_1 isbn ((l, _ , r, _): c)
    | isbn == l = not r
    |otherwise = func_1 isbn c

func_2 :: Isbn -> Emprestimo -> Int
func_2 _ [] = 0
func_2 isbn ((_ , l) : c)
    |isbn == l = 1 + func_2 isbn c
    |otherwise = func_2 isbn c

func_3 :: Isbn -> Acervo -> Int
func_3 _ [] = 0
func_3 isbn ((l, _ , _, q): c)
    | isbn == l = q
    |otherwise = func_3 isbn c

func_4 :: Isbn -> Int
func_4 isbn = func_3 isbn acervo - func_2 isbn emprestimo

func_5 :: Matricula -> Isbn -> Emprestimo
func_5 matr is
    |func_1 is acervo = (matr, is) : emprestimo
    |otherwise = emprestimo