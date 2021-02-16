--IGNACIO DE LA CRUZ CREPO
--1
--a)
lastFold :: [b] -> b
lastFold (x:xs) = foldl (\_ x -> x) x xs
--b)
reverseFold :: Foldable t => t a -> [a]
reverseFold zs = foldl (\xs y -> (y:xs)) [] zs
--c)
allFold :: Foldable t1 => (t2 -> Bool) -> t1 t2 -> Bool
allFold fun = foldl (\x y -> fun y && x) True
--d)
minimumFold :: Ord b => [b] -> b
minimumFold (x:xs) = foldr min x xs
--e)
mapFold :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
mapFold fun = foldl (\x y -> x ++ [fun y]) []
--f)
filterFold :: Foldable t => (a -> Bool) -> t a -> [a]
filterFold fun xs = foldl (\x y -> filterFoldAux fun x y) [] xs
filterFoldAux :: (a -> Bool) -> [a] -> a -> [a]
filterFoldAux fun x y | fun y = x ++ [y]
                      | otherwise = x
--g)
takeWhileFold :: Foldable t => (a -> Bool) -> t a -> [a]
takeWhileFold fun = foldr (\x y -> takeWhileFoldAux fun x y) []
takeWhileFoldAux :: (a -> Bool) -> a -> [a] -> [a]
takeWhileFoldAux fun x y | fun x = x:y
                         | otherwise = []
--2
foldl11 :: (t -> t -> t) -> [t] -> t
foldl11 fun (x:xs) = foldl11Aux fun (reverse xs) 
foldl11Aux :: (t -> t -> t) -> [t] -> t
foldl11Aux fun (x:[]) = x
foldl11Aux fun (x:xs) = fun x (foldl11Aux fun xs)

foldr11 :: (t -> t -> t) -> [t] -> t
foldr11 fun (x:[]) = x
foldr11 fun (x:xs) = fun x (foldr11 fun xs)
--3
--a)
lInt :: [Integer]
lInt =  concat [ x:x*(-1):[]  | x<-[1..]]
--b)
listaInfinita :: [(Integer, Integer)]
listaInfinita = [(x,y) | x<-[0..], y<-[0..x]]
--4
--a)
sufijos :: [a] -> [[a]]
sufijos xs = iterate init xs ++ [[]]
--b)
sublistas :: [a] -> [[a]]
sublistas (x:xs) = [x:ys | ys <- sublistas xs] ++ sublistas xs
sublistas [] = [[]]
--c)
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x:y | x <- xs, y <- permutaciones((filter (/=x) xs)) ]
--d)
sumandos :: (Num a, Enum a) => Int -> [[a]]
sumandos x = sumandosAux(take (x) [1,1..])

sumandosAux :: Num a => [a] -> [[a]]
sumandosAux (x:[]) = [[x]]
sumandosAux xs = [xs] ++ (sumandosAux (take (length xs - 2) xs ++ [sum(drop (length xs - 2) xs)]))
