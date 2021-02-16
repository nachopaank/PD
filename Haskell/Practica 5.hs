--IGNACIO DE LA CRUZ CRESPO
import System.Random
--1
getInt :: IO Integer
getInt = readLn

adivina :: IO ()
adivina = do
    num <- randomRIO (0, 100) :: IO Integer
    let adivinaAux = do 
         putStrLn "Escribe un entero"
         n <- getInt
         case compare n num of
          LT -> do
            putStrLn "Muy pequeño"
            adivinaAux
          GT -> do
            putStrLn "Muy grande"
            adivinaAux
          EQ -> do
            print("Lo encontraste")
    adivinaAux
--2
formatea :: FilePath -> FilePath -> Int -> IO ()
formatea fileIn fileOut n = do 
    defFile <- readFile fileIn
    let toWrite = allLines (lines defFile) n
    writeFile fileOut toWrite

allLines :: [[Char]] -> Int -> [Char]
allLines [] n = ""
allLines (x:xs) n = currentLine x n ++ "\n" ++ allLines xs n


currentLine :: [Char] -> Int -> [Char]
currentLine x n | (length x) < n = modifyLine x ((n-(length x)) `mod` (countSpaces x ' ')) ((n-(length x)) `div` (countSpaces x ' '))
         | otherwise = x

countSpaces :: Eq a => [a] -> a -> Int
countSpaces str c = length $ filter (== c) str

modifyLine :: (Ord a, Num a, Num t, Eq t) => [Char] -> a -> t -> [Char]
modifyLine [] n m= []
modifyLine (x:xs) n m| x == ' ' && n == 0 = ' ':(addNspaces m ++ modifyLine xs n m)
                   | x == ' ' && n>0  = ' ':' ': (addNspaces m ++ modifyLine xs (n-1) m)
                   | otherwise = x : modifyLine xs n m

addNspaces :: (Num t, Eq t) => t -> [Char]
addNspaces 0 = ""
addNspaces x  = " "++addNspaces (x-1)
--3
data Matriz a = Matriz [[a]] deriving (Eq)
sumaMat :: Num c => [[c]] -> [[c]] -> [[c]]
sumaMat [] [] = []
sumaMat (x:xs) (y:ys) = (zipWith (+) x y):[]++sumaMat xs ys

transMat :: Eq a => [[a]] -> [[a]]
transMat a | (a!!0) == [] = []
           | otherwise = (map head a) : transMat (map tail a)

prodMat :: (Num t, Eq t) => [[t]] -> [[t]] -> [[t]]
prodMat m1 m2 = [[sum $ zipWith (*) m1Aux m2Aux | m2Aux <- (transMat m2)] | m1Aux <- m1]

dibujaMatriz :: (Show a, Eq a) => [[a]] -> IO ()
dibujaMatriz x = mapM_ print(dibujaMatrizAux x)

dibujaMatrizAux :: (Show a, Eq a) => [[a]] -> [[Char]]
dibujaMatrizAux [] = []
dibujaMatrizAux (x:xs) | x == [] = [] 
                    | otherwise = (miLinea x):[] ++ dibujaMatrizAux xs
                    
miLinea :: Show a => [a] -> [Char]
miLinea [] = []
miLinea (m:ms) = (show m)++" "++miLinea ms

