{-
    4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
-}

{-
    a) (Int -> Int) -> Int
-}
auxiliarA :: Int -> Int
auxiliarA x = x + 1

sumar3A :: (Int -> Int) -> Int
sumar3A f = f 3

-- funcion auxiliar para usar en el ejemplo a
opuestoA :: Int -> Int
opuestoA x = -x

valorAbsolutoA :: (Int -> Int) -> Int
valorAbsolutoA f = f (-3)

{-
    b) Int -> (Int -> Int)
-}
addB :: Int -> (Int -> Int)
addB x y = x + y

productoB :: Int -> (Int -> Int)
productoB x y = x * y

{-
    c) (Int -> Int) -> (Int -> Int)
-}
auxiliarC :: Int -> Int
auxiliarC x = x * 2

ejemploC :: (Int -> Int) -> (Int -> Int)
ejemploC f x = f (f x)

-- funcion auxiliar para usar en el ejemplo c
potenciaC :: Int -> Int
potenciaC x = x * x

ejemC :: (Int -> Int) -> (Int -> Int)
ejemC f x = f (f x) * 3

{-
    d) Int -> Bool
-}
esParD :: Int -> Bool
esParD x = if x `mod` 2 == 0 then True else False

esPositivoD :: Int -> Bool
esPositivoD x = if x > 0 then True else False

{-
    e) Bool -> (Bool -> Bool)
-}
andE :: Bool -> (Bool -> Bool)
andE x y = x && y

orE :: Bool -> (Bool -> Bool)
orE x y = x || y

{-
    f) (Int,Char) -> Bool
-}
ejemF :: (Int, Char) -> Bool
ejemF (x, y)
  | x == 10 && y == 'a' = True
  | otherwise = False

turnosF :: (Int, Char) -> Bool
turnosF (x, y)
  | x > 7 && x < 19 && y /= 'D' = True
  | otherwise = False

{-
    g) (Int,Int) -> Int
-}
sumaG :: (Int, Int) -> Int
sumaG (x, y) = x + y

potenG :: (Int, Int) -> Int
potenG (x, y) = x ^ y

{-
    h) Int -> (Int,Int)
-}
alPlanoH :: Int -> (Int, Int)
alPlanoH x = (x, 2 * x)

alCuboH :: Int -> (Int, Int)
alCuboH x = (x, x ^ 3)

{-
    i) a -> Bool
-}
esVerdadI :: a -> Bool
esVerdadI x = True

esFalsoI :: a -> Bool
esFalsoI x = False

{-
    j) a -> a
-}
identidadJ :: a -> a
identidadJ x = x

ejemJ :: a -> a
ejemJ x = x