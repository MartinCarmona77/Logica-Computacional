--
--

data Natural = Cero | Suc Natural deriving (Eq, Show)

instance Ord Natural where
  Cero <= _ = True
  (Suc n) <= (Suc m) = n <= m
  _ <= _ = False

mayorQue :: Natural -> Natural -> Bool
mayorQue x y = if x > y
               then True
               else False

menorQue :: Natural -> Natural -> Bool
menorQue x y = if x < y
               then True
               else False

igual :: Natural -> Natural -> Bool
igual x y = if x == y
            then True
            else False


data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving Show

--instance Show ListaDeNaturales where
--  show Nil = "Nil"
--  show Cons Natural ListaDeNaturales = "Cons" ++ Natural ++ "Nil"

concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate Nil x = x
