{-
2. Definir las siguientes funciones y determinar su tipo:
-}

{-
    a) five, que dado cualquier valor, devuelve 5
-}
five :: a -> Int
five x = 5
-- toma un valor de cualquier tipo y devuelve el entero "5"

{-
    b) apply, que toma una función y un valor, y devuelve el resultado de
    aplicar la función al valor dado
-}
apply :: (t1 -> t2) -> t1 -> t2
apply f x = f x
-- la funcion es como un map de un solo valor
-- la funcion que se aplica se tiene que poder aplicar a un solo valor y arrjar
-- un resultado

{-
    c) identidad, la función identidad
-}
identidad :: Int -> Int
identidad x = x
-- en este caso la funcion identidad devuelve lo mismo que recibe
-- pero vamos a respetar el concepto de funcion matematica que recibe un numero

{-
    d) first, que toma un par ordenado, y devuelve su primera componente
-}
first :: (a, b) -> a
first (x, y) = x
-- toma un par ordenado o tupla, y devuelve su primera componente

{-
    e) derive, que aproxima la derivada de una función dada en un punto dado
-}
derive :: (Fractional a, Fractional t) => (t -> a) -> t -> a
derive f x = (f (x + 0.00001) - f x) / 0.00001

{-
    f) sign, la función signo
-}
sign :: Int -> Int
sign x
    | x > 0 = 1
    | x < 0 = -1
    |otherwise = 0

sign2 :: Int -> String
sign2 x
    | x > 0 = "Positivo"
    | x < 0 = "Negativo"
    |otherwise = "0 no tiene signo"

{-
    g) vabs, la función valor absoluto (usando sign y sin usarla)
-}
vabs :: Int -> String
vabs x
    | resul == 1 = "Positivo"
    | resul == -1 = "Negativo"
    | otherwise = "0 no tiene signo"
        where resul = sign x

{-
    h) pot, que toma un entero y un número, y devuelve el resultado de
    elevar el segundo a la potencia dada por el primero
-}
pot :: Int -> Int -> Int
pot x y = y ^ x

{-
    i) xor, el operador de disyunción exclusiva
-}
xor :: Bool -> Bool -> Bool
xor x y = if x == y then False else True

{-
    j) max3, que toma tres números enteros y devuelve el máximo entre llos
-}
max3 :: Int -> Int -> Int -> Int
max3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

{-
    k) swap, que toma un par y devuelve el par con sus componentes invertidas
-}
swap :: (a, a) -> (a, a)
swap (x , y) = (y, x)