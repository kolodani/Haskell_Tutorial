{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}

-- a) 'suma', que suma todos los elementos de una lista de números
suma :: [Int] -> Int
suma xs = foldr (+) 0 xs

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario
alguno :: [Bool] -> Bool
alguno xs = foldr (||) False xs

-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario
todos :: [Bool] -> Bool
todos xs = foldr (&&) True xs

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales
codes :: [Char] -> [Int]
codes xs = map (+ (-96)) (map fromEnum xs)

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado
restos :: Int -> [Int] -> [Int]
restos n xs = map (`mod` n) xs

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados
cuadrados :: [Int] -> [Int]
cuadrados xs = map (^ 2) xs

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes
longitudes :: [[a]] -> [Int]
longitudes xs = map length xs

-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda
orden :: [(Int, Int)] -> [(Int, Int)]
orden xs = filter (\(x, y) -> x < 3 * y) xs

-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares
pares :: [Int] -> [Int]
pares xs = filter even xs

-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)
letras' :: [Char] -> [Char]
letras' xs = filter (\x -> (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')) xs

-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n'
masDe :: Int -> [[Int]] -> [[Int]]
masDe n (x : xs) = filter (\x -> length x > n) xs
