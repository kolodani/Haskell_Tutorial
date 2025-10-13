import Data.List

{-
1)  Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs
    en el interprete de Haskell GHCi, leer los mensajes de error y corregirlos
    hasta que el archivo se cargue correctamente.
-}

-- a)
-- regla b = case b of
--    True -> "Quedate en Casa"
--    False -> "Qudate en Casa"
regla :: Bool -> String
regla x = if x then "Quedate en Casa" else "Sali a Pasear"
{-
    preferia hacer un condicional mas que usar el case ya que la condicion es booleana y se podia
    resolver en una sola linea
-}

-- b)
-- case [x]         =  []
-- case (x:y:xs)      =  y : case (x:xs)
-- case []          =  []
case1 :: [a] -> [a]
case1 [x]         =  []
case1 (x:y:xs)      =  y : case1 (x:xs)
case1 []          =  []
{-
    en este caso la palabra "case" es una palabra reservada, lo que hace que cuando lo queremos
    usar como nombre de funcion nos de un error ya que es ambiguo el uso que le estamos dando.
    el sistema no entiende que nosotros quisimos nombrar una funcion
-}

-- c)
-- map f []        =  []
-- map f (x:xs)     =  f x : map f xs
map1 :: (t -> a) -> [t] -> [a]
map1 f []        =  []
map1 f (x:xs)     =  f x : map1 f xs
{-
    con map nos pasa lo mismo, es una funcion que esta en el preludio,
    no es una palabra reservada, pero es una funcion que ya existe.
    lo que sucede que hay un uso ambiguo, no sabe que es lo que tiene que hacer
    si usarlo como marca el preludio o como lo estamos redefiniendo nosotros
-}

-- d)
-- listNumeros = (1 : 2) : 'a' : []
listNumeros = '1' : '2' : 'a' : []
{-
    las listas son de un unico tipo, en este caso la lista recibia valores de
    distintos tipos, lo que produce el error.
    las listas no reciben distintos tipos, esas son las tuplas.
-}

-- e)
-- []     ++! ys = ys
-- (x:xs) ++! ys = x : xs ++! ys
(++!) :: [a] -> [a] -> [a]
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys
{-
    la funcion esta bien declarada, solo le agrego la declaracion para que se
    sepa que toma dos listas, y devuelve una lista, de cualquier tipo
    lo que hace la misma es concatenarlas
-}

-- f)
-- addToTail x xs = map +x tail xs
addToTail :: Num b => b -> [b] -> [b]
addToTail x xs = map (+ x) (tail xs)
{-
    en este caso tenemos que acomodar un par de cosas
    map toma dos argumentos por eso tenemos que usar parentesis, para asegurarnos
    que tail tome una lista y que lo que reciba map sean una funcion y una lista
-}

-- g)
-- listmin xs = head . sort xs
listmin :: Ord c => [c] -> c
listmin xs = (head . sort) xs
{-
    la composicion de funciones se logra agragando los parentesis, porque head
    quiere tomar una lista pero antes esta la funcion sort, pero con los parentesis
    y el punto se le dice head, que recibe el resultado de aplicar sort a la lista
-}

-- h) (*)
-- smap f [] = []
-- smap f [x] = [f x]
-- smap f (x:xs) = f x : smap (smap f) xs
smap f [] = []
smap f [x] = [f x]
smap f (x : xs) = f x : smap f xs
{-
    corregimos la llamada recursiva ya que la funcion no puede llamarse a si misma
    de la forma que esta escrita, no tiene una aplicacion parcial.
    la funcion espera una funcion y una lista
-}