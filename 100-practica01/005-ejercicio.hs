{-
    5) Definir las siguientes funciones usando listas por comprensión:
-}

{-
    a) 'divisors', que dado un entero positivo 'x' devuelve la
    lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
-}
divisors :: Int -> [Int]
divisors x = [i | i <- [1..x], mod x i == 0]

{-
    b) 'matches', que dados un entero 'x' y una lista de enteros descarta
    de la lista los elementos distintos a 'x'
-}
matches :: Int -> [Int] -> [Int]
matches x ys = [y | y <- ys, x == y]

{-
    c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
    '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
    donde 0 <= a, b, c, d <= 'n'
-}
cuadruplas :: Int -> [(Int, Int, Int, Int)]
cuadruplas n = [(a, b, c, d) | a <- [1..n], b <- [1..n], c<- [1..n], d<- [1..n], a^2 + b^2 == c^2 + d^2]

{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}
unique :: [Int] -> [Int]
unique [] = []
unique [x] = [x]
unique (x:xs) = x : unique [y | y <- xs, y /= x]