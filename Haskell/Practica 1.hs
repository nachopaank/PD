--Ignacio de la Cruz Crespo

--1.

--a)10^10/60/60/24/365
--b)anos enteros
--  floor (10^10/60/60/24/365) 
-- 317

--  dias restantes
--  floor(fromIntegral(mod (10^10) (60*60*24*365)) / 60/60/24)
-- 35

--  horas restantes
--  floor(fromIntegral(mod (10^10) (60*60*24)) / 60/60)
-- 17

--  minutos restantes
--  floor(fromIntegral(mod (10^10) (60*60)) / 60)
-- 46

--  segundo restantes
--  floor(fromIntegral(mod (10^10) (60)))
--  40


--c)
anos:: Fractional a => a -> a
anos x = x/60/60/24/365

anosenteros:: Integral b => RealFrac a => a -> b
anosenteros x = floor (x/60/60/24/365) 

diasrestantes:: Integral a => a -> a
diasrestantes x = floor(fromIntegral(mod (x) (60*60*24*365)) / 60/60/24)

horasrestantes:: Integral a => a -> a
horasrestantes x = floor(fromIntegral(mod (x) (60*60*24)) / 60/60)

minutosrestantes:: Integral a => a -> a
minutosrestantes x = floor(fromIntegral(mod (x) (60*60)) / 60)

segundosrestantes:: Integral a => a -> a
segundosrestantes x = floor(fromIntegral(mod (x) (60)))
--2.

-- last [1..10^5] poco
-- last [1..10^7] regular
-- last [1..10^20] mucho
-- head [1..10^20] poco
-- last [10^20..1] exception
-- head (tail [1..10^20]) poco
-- length [1..10^20] mucho
-- last (take (10^7) [1..10^20]) regular
-- head (take (10^7) ([1..100] ++ [1..10^20])) poco
-- last (take 100 ([1..10^20] ++ [1..100])) poco
-- last (drop 100 ([1..10^20] ++ [1..100])) mucho
-- head (drop (10^7) ([1..10^20] ++ [1..100])) regular
-- [1..10^7]==[1..10^7] regular
-- [1..10^20]==[1..10^20] mucho
-- [1..10^20]==[1..10^20+1] mucho
-- [1..10^20]==[2..10^20] poco
-- head (reverse [1..10^7]) regular
-- last (reverse [1..10^7]) regular
-- reverse [1..10^20] == reverse [1..10^20+1] mucho

--3 El problema es que el tipo de length es int y suma devuelve un num

media:: Integral a => [a] -> a
media xs = (suma xs) `div` fromIntegral(length xs)

suma:: Num a => [a] -> a
suma [x] = x
suma (x:xs) = x + suma xs

--4

digitos:: Integral a => a -> a
digitos x | (x >= 10) = (1 + digitos (x `div` 10))
          | otherwise = 1

digitosSuma:: Integral a => a -> a
digitosSuma x | (x >= 10) = (x - ((x `div` 10) * 10)) + digitosSuma (x `div` 10)
              | otherwise = x

reduccion:: Integral a => a -> a
reduccion x | (x >= 10) = reduccion (digitosSuma x)
            | otherwise = x

perm:: Integral a => a -> a
perm x | x == 0 = 1
       | x > 0 = x*(perm (x - 1))

var:: Integral a => a -> a -> a
var n m = perm m `div` perm (m - n)

comb:: Integral a => a -> a -> a
comb n m = var n m `div` perm n

--5

conjuncion1:: Bool -> Bool -> Bool
conjuncion1 True True  = True
conjuncion1 _ _ = False
-- no estricta 

conjuncion2:: Bool -> Bool -> Bool
conjuncion2 True True  = True
conjuncion2 True False = False
conjuncion2 False _ = False
-- estricto en el primero

conjuncion3:: Bool -> Bool -> Bool
conjuncion3 True True  = True
conjuncion3 _ False = False
-- estricto en el segundo

conjuncion4:: Bool -> Bool -> Bool
conjuncion4 True True  = True
conjuncion4 True False = False
conjuncion4 False True = False
conjuncion4 False False = False
-- estricto en los 2

