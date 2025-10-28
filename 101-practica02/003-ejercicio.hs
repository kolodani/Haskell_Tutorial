{-
    3. Dar dos ejemplos de funciones que tengan los siguientes tipos:
-}

-- a) (Int → Int) → (Bool → Bool)
-- en este caso, la función recibe una función que recibe un entero y devuelve un entero, y devuelve una función que recibe un booleano y devuelve un booleano
-- función auxiliar para usar en el ejemplo a
signoNumero :: Int -> Int
signoNumero x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

-- función que recibe una función que recibe un entero y devuelve un entero, y devuelve una función que recibe un booleano y devuelve un booleano
signoBool :: (Int -> Int) -> (Bool -> Bool)
signoBool f a
    | a = f 1 > 0
    | otherwise = f 0 > 0

esCero :: (Int -> Int) -> (Bool -> Bool)
esCero f a
    | a = f 0 == 0
    | otherwise = f 1 == 0

-- b) Bool → (Int → Bool)
-- en este caso, la función recibe un booleano y devuelve una función que recibe un entero y devuelve un booleano
-- función auxiliar para usar en el ejemplo b
esPar :: Int -> Bool
esPar x = mod x 2 == 0

-- función que recibe un booleano y devuelve una función que recibe un entero y devuelve un booleano
esParBool :: Bool -> (Int -> Bool)
esParBool a x
    | a = esPar x
    | otherwise = not (esPar x)

esImparBool :: Bool -> (Int -> Bool)
esImparBool a x
    | a = not (esPar x)
    | otherwise = esPar x

-- c) Char → Char
convertirA :: Char -> Char
convertirA x = 'a'

echo :: Char -> Char
echo x = x

-- d) Int → (Int → Bool) → [Int]
-- en este caso, la función recibe un entero y devuelve una función que recibe un entero y devuelve un booleano, y devuelve una lista de enteros
-- función auxiliar para usar en el ejemplo d
esPar' :: Int -> Bool
esPar' x = mod x 2 == 0

-- función que recibe un entero y devuelve una función que recibe un entero y devuelve un booleano, y devuelve una lista de enteros
esParLista :: Int -> (Int -> Bool) -> [Int]
esParLista x f = [y | y <- [1 .. x], f y]

esImparLista :: Int -> (Int -> Bool) -> [Int]
esImparLista x f = [y | y <- [1 .. x], not (f y)]

-- e) [a ] → (a → [b ]) → [b ]
-- en este caso, la función recibe una lista de elementos de tipo a y una función que recibe un elemento de tipo a y devuelve una lista de elementos de tipo b, y devuelve una lista de elementos de tipo b
-- función auxiliar para usar en el ejemplo e
listar :: Int -> [Int]
listar x = [1 .. x]

echo' :: Char -> [Char]
echo' x = [x, x]

-- función que recibe una lista de elementos de tipo a y una función que recibe un elemento de tipo a y devuelve una lista de elementos de tipo b, y devuelve una lista de elementos de tipo b
listarFuncion :: [Int] -> (Int -> [Int]) -> [Int]
listarFuncion xs f = concat [f x | x <- xs]

listarCaracteres :: [Char] -> (Char -> [Char]) -> [Char]
listarCaracteres xs f = concat [f x | x <- xs]

-- f ) [[a ]] → (a → Bool) → [a ]
-- en este caso, la función recibe una lista de listas de elementos de tipo a y una función que recibe un elemento de tipo a y devuelve un booleano, y devuelve una lista de elementos de tipo a
-- función auxiliar para usar en el ejemplo f
esCinco :: Int -> Bool
esCinco x = x == 5

esNatural :: Int -> Bool
esNatural x = x > 0

-- función que recibe una lista de listas de elementos de tipo a y una función que recibe un elemento de tipo a y devuelve un booleano, y devuelve una lista de elementos de tipo a
esCincoLista :: [[Int]] -> (Int -> Bool) -> [Int]
esCincoLista xs f = concat [y | y <- xs, f (head y)]

esNaturalLista :: [[Int]] -> (Int -> Bool) -> [Int]
esNaturalLista xs f = concat [y | y <- xs, f (head y)]

-- g) (a, b, c) → Bool
-- en este caso, la función recibe una tupla de tres elementos de tipo a y devuelve un booleano
positivos :: (Int, Int, Int) -> Bool
positivos (a, b, c)
    | a > 0 && b > 0 && c > 0 = True
    | otherwise = False

repetidas :: (Char, Char, Char) -> Bool
repetidas (a, b, c)
    | a == b || a == c || b == c = True
    | otherwise = False

-- h) (a, b, c) → Int → c
-- en este caso, la función recibe una tupla de tres elementos de tipo a y devuelve una función que recibe un entero y devuelve un elemento de tipo c
igualitos :: Int -> Int -> Int
igualitos x y
    | x == y = 1
    | otherwise = 0

elementoIgual :: (Int, Int, Int) -> Int -> Int
elementoIgual (a, b, c) x = sum [igualitos a x, igualitos b x, igualitos c x]

multiplicar :: (Int, Int, Int) -> Int -> Int
multiplicar (a, b, c) x = a * x + b * x + c * x

-- i) (a, a, a) → Int → a
-- en este caso, la función recibe una tupla de tres elementos de tipo a y devuelve una función que recibe un entero y devuelve un elemento de tipo a
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

usoXor :: (Bool, Bool, Bool) -> Int -> Bool
usoXor (a, b, c) x
    | x == 1 = xor a b
    | x == 2 = xor b c
    | x == 3 = xor a c
    | otherwise = False

usoXor' :: (Bool, Bool, Bool) -> Int -> Bool
usoXor' (a, b, c) x
    | x == 1 = xor' a b
    | x == 2 = xor' b c
    | x == 3 = xor' a c
    | otherwise = False
