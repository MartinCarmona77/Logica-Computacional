pote :: Int->Int->Int
pote x 0 = 1
pote x y = x * (pote x (y-1))

poteB :: Int -> Int -> Int
poteB x y = case y of
              0 -> 1
              _ -> if y > 0
                then x * (poteB x (y - 1))
                else error "Solo positivos"

toma :: Int -> [a] -> [a]
toma 0 _ = []
toma _ [] = []
toma n (x:xs) = [x] ++ toma (n-1) xs

mayores :: Ord a => [a] -> a -> [a]
mayores l n = [x | x <-l, x > n]

data Natural = Cero | Suc Natural deriving (Eq,Show)

suma :: Natural -> Natural -> Natural
suma Cero x = x
suma (Suc x) y = Suc(suma x y) 

suma1 :: Natural -> Natural -> Natural
suma1 x y = case y of
              Cero -> x
              Suc z -> Suc(suma x z)

prod :: Natural -> Natural -> Natural
prod x y = case y of
             Cero -> Cero
             Suc z -> suma1 x (prod x z)

potN :: Natural -> Natural -> Natural
potN x y = case y of
             Cero -> Suc Cero
             Suc z -> prod x (potN x z)
