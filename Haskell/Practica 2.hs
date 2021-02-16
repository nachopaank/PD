--IGNACIO DE LA CRUZ CRESPO

--1. a)
cuadrados:: Integral a => a -> [a]
cuadrados x = c2 [0..x] []
c2 :: Num a => [a] -> [a] -> [a]
c2 [] ys = ys
c2 (x:xs) ys = c2 xs ((x^2:ys))

--1. c)
sumacos :: (Ord a, Floating a) => a -> a
sumacos z = s2 1 z
s2 :: (Ord a, Floating a) => a -> a -> a
s2 x y | (x < y) = x*(abs (cos x)) + (s2 (x+1) y)
       | otherwise = x*(abs (cos x))

--1. f)
primo :: Integral t => t -> t
primo x | (esprimo x == True) = x
        | otherwise = primo (x+1)
esprimo :: Integral t => t -> Bool
esprimo x = [y | y <- [1..x], x `mod` y == 0] == [1,x]

--1. d)
sumamen :: Integral a => a -> a
sumamen x = ss2 x 0

ss2 :: Integral a => a -> a -> a
ss2 x y | (x == 0) = 0
        |(x `mod` 3 == 0) = y + x +(ss2 (x-1) y)
        |(x `mod` 5 == 0) = y + x +(ss2 (x-1) y)
        | otherwise = y + (ss2 (x-1) y)
--2. a)
cuadradoSup :: (Num b, Enum b) => b -> [b]
cuadradoSup x = map (^2) [0..x]
--2. b)
cuadradoRev :: (Num b, Enum b) => b -> [(b, b)]
cuadradoRev x = zip [x,x-1..0] (map(^2) [x,x-1..0])
--2. c)
sumacosSup :: (Floating a, Enum a) => a -> a
sumacosSup x = sum (zipWith (*) [1..x] (map(abs) (map(cos) [1..x])))
--3. a)
iguales  :: (Ord a, Num a, Eq b) => (a -> b) -> (a -> b) -> a -> a -> Bool
iguales f g n m | (m > n && f m == g m) = iguales f g n (m-1)
                | (n == m && f m == g m) = True
                | otherwise = False

--3. b)
menorA :: (Ord t, Num t) => t -> t -> (t -> Bool) -> t
menorA n m p | (m > n && not (p n)) = menorA (n+1) m p
             | (m == n && not (p n)) = -1 --error, nadie lo cumple
             | otherwise = n
--3. c)
mayor :: Num t => t -> (t -> Bool) -> t
mayor n p | (not (p n)) = mayor (n-1) p
          | otherwise = n

--3. d)
ex :: (Ord a, Num a) => a -> a -> (a -> Bool) -> Bool
ex n m p | (m > n && not (p m)) = ex n (m-1) p
         | (m == n && not (p m)) = False
         | otherwise = True
--4. a)
filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q = (filter p xs, filter q xs)

--4. b)
filters :: [a] -> [a -> Bool] -> [[a]]
filters xs [] = []
filters xs (p:ps) = (filter p xs):(filters xs ps)