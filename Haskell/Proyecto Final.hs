--IGNACIO DE LA CRUZ CRESPO

--Para ver en forma de tabla las relaciones usar "mostrar", por ejemplo "mostrar r1"
--Para probar cualquier funcion escribir su nombre y despues la relecion de ejemplo, por ejemplo "relEquivalencia r1"

data Rel a = R [(a,a)]
--r1 no es de equivalencia
--r2 sirve para comprobar que a la hora de mostrar la tabla esta se formatea bien (distintos tamaños)
--r3 es de equivalencia
--r5 es el segundo ejemplo del enunciado
--r4 y r6 son iguales definidas de forma distinta
r1 = R([("D", "E"), ("E", "B"), ("C", "B"), ("E", "C")])
r2 = R([("4567", "44"), ("E22", "2222222D"), ("4D", "44"), ("4567", "EEEEEEE")])
r3 = R([("1","1"),("1","3"),("3","1"),("3","3"),("5","5")])
r4 = R([("A", "B"), ("B", "C"), ("B", "D"), ("C", "D")])
r5 = R([("[1,2]","[]"),("[2,-2]","[3,3,1]"),("[1,3]","[0]"),("[4]","[4]")])
r6 = R([("B", "C"),("A", "B"), ("C", "D"),("B", "D")])

--EJERCICIO 1

esRelacion :: Eq a => Rel a -> Bool
esRelacion r  = esRelacionAux r

esRelacionAux :: Eq a => Rel a -> Bool
esRelacionAux (R([])) = True
esRelacionAux (R(x:xs)) | elem x xs = False
                        | otherwise = esRelacionAux (R(xs))

--EJERCICIO 2

instance Show a => Show (Rel a) where
    show (R a) = show(a)


instance Eq a => Eq (Rel a) where
    (==) (R a) (R b) = if a == b then True
                       else False

--EJERCICIO 3.1
dominio :: Eq t => Rel t -> [t]
dominio r = duplicados(dominioAux(r))

dominioAux :: Rel t -> [t]
dominioAux (R(r)) = [ x | (x,y) <- r]

--EJERCICIO 3.2
soporte :: Eq t => Rel t -> [t]
soporte r = duplicados(soporteAux(r))

soporteAux :: Rel t -> [t]
soporteAux (R(r)) = [ z | (x,y) <- r, z <- x:y:[]]

--EJERCICIO 3.3
--Para que una relacion sea de equivalencia debe ser reflexiva,simetrica y transitiva
reflexiva :: Eq t => Rel t -> Bool
reflexiva (R(r)) = and [elem (x,x) r | (x,_) <- r]

simetrica :: Eq t => Rel t -> Bool
simetrica (R(r)) = and [elem (y,x) r | (x,y) <- r] 

transitiva :: Eq t => Rel t -> Bool
transitiva (R(r)) = and [(y == w && elem (x,z) r)||y/=w | (x,y) <- r,(w,z) <- r] 

--relEquivalencia :: Eq t => Rel t -> Bool
relEquivalencia r = simetrica r && reflexiva r && transitiva r

--EJERCICIO 3.4
conjCociente :: Eq t => Rel t -> [[t]]
conjCociente r | relEquivalencia r == True = duplicados(conjCocienteAux(r,dominio r))
               | otherwise = error "No es de equivalencia"

conjCocienteAux :: Eq a => (Rel a, [a]) -> [[a]]
conjCocienteAux ((R(y)),[]) = []
conjCocienteAux ((R(y)),(x:xs)) = conjCocienteAux2((R(y)),x):[] ++ conjCocienteAux((R(y)),xs)

conjCocienteAux2 :: Eq t => (Rel t, t) -> [t]
conjCocienteAux2((R(r)),n) = [ y | (x,y) <- r, (x==n)]

--EJERCICIO 3.5
generaDiv :: Integral a => a -> a -> Rel a
generaDiv n m = R([(a,b) | a <- [n..m], b <- [n..m],(a `mod` b)== 0])

--EJERCICIO 3.6
generaGE :: Ord a => [a] -> Rel a
generaGE xs = R([(a,b)  | a <- xs, b <- xs, a>=b])

--EJERCICIO 3.7
composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R(r1)) (R(r2)) = R([(a,d) | (a,b) <- r1, (c,d) <- r2, b == c])


--FUNCION UTIL PARA EJERCICIO 3
--Elimina los elementos duplicados de una lista
duplicados :: Eq t => [t] -> [t]
duplicados [] = []
duplicados (x:xs) = x : duplicados (filter (/= x) xs)

--EJERCICIO 4
muestraRel :: IO ()
muestraRel = do 
    x <- introRel []
    mostrar(x)

introRel :: [(String, String)] -> IO (Rel String)
introRel l = do
  putStrLn "Escribe el primer elemento del par | -1 para salir"
  userInput <- getLine
  if userInput == "-1" then do
     putStrLn "Saliendo.."
     --mostrar(R(l))
     return (R(l))
  else do
     putStrLn "Escribe el segundo elemento del par"
     userInput2 <- getLine
     let ch = (userInput,userInput2):l 
     let l = ch
     introRel l

mostrar :: Rel [Char] -> IO ()
mostrar x = do 
    primeraLinea x
    segundayUltimaLinea x
    dibujaMatriz x
    segundayUltimaLinea x

primeraLinea :: Rel [Char] -> IO ()
primeraLinea x = putStrLn(formateaLinea(espacios(tableLength(soporte(x),0)+2),soporte(x)))

segundayUltimaLinea :: (Foldable t, Eq (t a)) => Rel (t a) -> IO ()
segundayUltimaLinea x = putStrLn(formateaBarra(espacios(tableLength(soporte(x),0)+1)++"--",soporte(x)))

--calculo donde deben estar los espacios en la primera linea
formateaLinea :: ([Char], [[Char]]) -> [Char]
formateaLinea(x,[]) = x
formateaLinea(x,(y:ys)) = formateaLinea(x++" "++y,ys)

--calculo donde deben estar los guiones en la segunda y ultima linea, que son iguales
formateaBarra :: Foldable t => ([Char], [t a]) -> [Char]
formateaBarra(x,[]) = x
formateaBarra(x,(y:ys)) = formateaBarra(x++"-"++barras(length(y))++"",ys)

--el conjunto de todas las parejas de elementos posibles
elems :: Eq t => Rel t -> [(t, t)]
elems r = [z | x <- soporte(r), y <- soporte(r), z <- [(x,y)]]

dibujaMatriz :: Rel [Char] -> IO ()
dibujaMatriz x = putStrLn(dibujaMatrizAux(x,elems(x),length(soporte(x))+1,length(soporte(x))+1,tableLength(soporte(x),0)))

--la matriz donde indicamos que posiciones estan marcadas o no
dibujaMatrizAux :: (Num a, Eq a) => (Rel [Char], [([Char], [Char])], a, a, Int) -> [Char]
dibujaMatrizAux(R(r),[],n,c,e) = "|"
dibujaMatrizAux(R(r),(x:xs),n,c,e) | n == 0 = "|\n" ++ dibujaMatrizAux(R(r),(x:xs),c,c,e) 
                                   | n == c && n /= 0 = fst(x) ++ espacios(e-length(fst(x))+1)++ "|" ++ dibujaMatrizAux(R(r),(x:xs),(n-1),c,e)
                                   | elem x r = espaciosConX(length(snd(x))+1) ++ dibujaMatrizAux(R(r),xs,(n-1),c,e)
                                   | otherwise = espacios(length(snd(x))+1) ++ dibujaMatrizAux(R(r),xs,(n-1),c,e)

--anadir e espacios al string calculado previamente puesto que depende de la longitud del elemento
espacios :: (Num t, Eq t) => t -> [Char]                    
espacios(0) = ""
espacios(e) = " "++espacios(e-1)

--identica a la anterior salvo que el elemento si existe
espaciosConX :: Integral t => t -> [Char]
espaciosConX(0) = ""
espaciosConX(e) | mod e 2 == 1 = espacios(div e 2) ++ "X" ++ espacios(div e 2)
                | otherwise = espacios(div e 2) ++ "X" ++ espacios((div e 2)-1)

--anadir guiones exactos para encuadrar la matriz
barras :: (Num t, Eq t) => t -> [Char]
barras(0) = ""
barras(e) = "-"++barras(e-1)

--la longitud del elemento mas largo para poder formatear la matriz
tableLength :: Foldable t => ([t a], Int) -> Int
tableLength ([],n) = n
tableLength ((x:xs),n) | length x > n = tableLength(xs,length(x))
                       | otherwise = tableLength(xs,n)