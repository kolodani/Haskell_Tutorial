{-
    1. Definir las siguientes funciones en forma recursiva:
-}

-- a) borrarUltimo que dada una lista borra el último elemento de la lista. No utilizar reverse, ni tail.
borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

-- b) collect :: [(k , v )] → toma un lista de pares (clave, valor) y asocia cada clave única con todos
-- los valores con los que estaba apareada originalmente. Por ejemplo: collect
collect :: [Char] -> [([Char], Int)] -> [Int]
collect _ [] = []
collect a ((k, v): xs) = if a == k then v : collect a xs else collect a xs

-- c) serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]]
-- Dar su tipo más general.
seriar :: [Int] -> [[Int]]
seriar [] = []
seriar xs = xs : seriar (borrarUltimo xs)

serie :: [Int] -> [[Int]]
serie [] = []
serie xs = [] : reverse (xs : seriar (borrarUltimo xs))

-- d) paresIguales :: Int → Int → Int → Int → Bool toma 4 números enteros y retorna True si de
-- dos en dos son iguales (en cualquier orden), en los demás casos retorna False. Por ejemplo:
-- paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
-- paresIguales 3 1 1 3 = True
paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d
    | a == b && a == c && a == d = False
    | a == b && c == d = True
    | a == c && b == d = True
    | a == d && b == c = True
    | otherwise = False

-- e) isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un triángulo nos
-- dice si es un triángulo isósceles.
isosceles :: Int -> Int -> Int -> Bool
isosceles a b c
    | a == b && a /= c = True
    | a == c && a /= b = True
    | b == c && b /= a = True
    | otherwise = False

-- f ) ror que dada una lista xs y un entero n, tal que n ⩽ lenght xs, rota los primeros n elementos
-- de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. Definir una versión recursiva de ror,
-- sin usar drop, take ni tail .
cabeza :: Int -> [Int] -> [Int]
cabeza 0 [] = []
cabeza 0 xs = []
cabeza a (x:xs) = x: cabeza (a - 1) xs

cola :: Int -> [Int] -> [Int]
cola _ [] = []
cola a (x:xs) = if a > 0 then cola (a - 1) xs else x : cola (a - 1) xs

ror :: Int -> [Int] -> [Int]
ror n xs = cola n xs ++ cabeza n xs

-- g) upto :: Int → Int → [Int] que dado dos números enteros n y m devuelve la
-- lista [n, n + 1, n + 2, ..., m ] en caso que n ⩽ m y la lista [ ] en otro caso.
-- No usar listas por comprensión.
upto :: Int -> Int -> [Int]
upto a b
    | a > b = []
    | a <= b = a : upto (a + 1) b

-- h) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada
-- elemento tantas veces como indica su posición. No usar listas por comprensión.
-- Por ejemplo: eco "hola" = "hoolllaaaa"
cardinales :: [Char] -> [(Char, Int)]
cardinales xs = zip xs [1..]

reververar :: (Char, Int) -> [Char]
reververar (_, 0) = []
reververar (c, n) = c : reververar (c, n - 1)

eco :: [Char] -> [Char]
eco xs = foldr (++) [] (map (\(c, i) -> reververar (c, i)) (cardinales xs))
