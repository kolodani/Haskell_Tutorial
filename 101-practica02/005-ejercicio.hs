{-
    5. Definir las siguientes funciones usando foldr:
-}

-- a) map :: (a → b) → [a ] → [b ] que dada una función y una lista, aplica la función a cada
-- elemento de la lista.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- b) filter :: (a → Bool) → [a ] → [a ] , que dado un predicado y una lista xs, devuelve una lista
-- con los elementos de xs que satisfacen el predicado.
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x : xs else xs) []

-- c) unzip :: [(a, b)] → ([a ], [b ]), que dada una lista de tuplas xs retorna una tupla de listas donde
-- cada una corresponde a los primeros y secundos elementos de los pares respectivamente.
-- Ej. unzip [(’a’, 1), (’z’, 7), (’h’, 9)] = ("azh", [1, 7, 9])
unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], [])

-- d) pair2List ::(a, [b ]) → [(a, b)] que dado un par formado por un valor x y una lista xs convierta
-- a la lista xs en una lista de pares, formada con los elementos de xs y x .
-- Ej. pair2List (x , [y1 , y2 , y3 ]) = [(x , y1 ), (x , y2 ), (x , y3 )]
pair2List :: (a, [b]) -> [(a, b)]
pair2List (x, ys) = foldr (\y acc -> (x, y) : acc) [] ys

-- e) maxSec :: [(Int, Int)] → (Int, Int), que dada una lista de pares de naturales que represente a
-- una lista de segmentos de la recta, calcule el segmento más largo de la misma.
-- Ej.maxSec [(1, 2), (0, 7), (4, 6)] = (0, 7)
-- Puede definir una función auxiliar maxL :: (Int, Int) → (Int, Int) → (Int, Int), que dados dos
-- pares de naturales que representan a dos segmentos de la recta, devuelva el segmento cuya
-- longitud sea máxima.
-- Ej.maxL (1, 2) (0, 7) = (0, 7).
maxSec :: [(Int, Int)] -> (Int, Int)
maxSec = foldr maxL (0, 0)
    where
        maxL (x1, y1) (x2, y2) = if y1 - x1 > y2 - x2 then (x1, y1) else (x2, y2)