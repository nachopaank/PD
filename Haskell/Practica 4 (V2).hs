--Ignacio de la Cruz Crespo
data Direc = Arriba | Abajo | Derecha | Izquierda deriving (Enum, Show, Eq, Ord)
destino :: Num a => a -> a -> [Direc] -> [a]
destino x y [] = [x,y]
destino x y (l:ls) | l == Arriba =  destino x (y+1) ls
                   | l == Abajo = destino x (y-1) ls
                   | l == Derecha = destino (x+1) y ls
                   | l == Izquierda = destino (x-1) y ls
trayectoria :: Num t => t -> t -> [Direc] -> [[t]]
trayectoria x y [] = []
trayectoria x y (l:ls) = (destino x y [l]) : trayectoria ((destino x y [l])!!0) ((destino x y [l])!!1) ls 

destino' :: (Ord a, Num a) => a -> a -> a -> [Direc] -> [a]
destino' n x y [] = [x,y]
destino' n x y (l:ls) | l == Arriba && y < n =  destino' n x (y+1) ls
                      | l == Abajo && y > 0 = destino' n x (y-1) ls
                      | l == Derecha  && x < n= destino' n (x+1) y ls
                      | l == Izquierda && x > 0= destino' n (x-1) y ls
                      | otherwise = destino' n x y ls
inferior :: (Ord t, Num t, Enum t) => t -> [Direc] -> [Direc] -> Bool
inferior n ls ks = foldl (&&) True (map (inferiorAux n ls ks) [[i,j]|i <- [1..n], j <- [1..n]])

inferiorAux :: (Ord a, Num a) => a -> [Direc] -> [Direc] -> [a] -> Bool
inferiorAux n (l:ls) (k:ks) (x:y:[]) | ((destino x y [l])!!1 >= (destino x y [k])!!1) && inferiorAux n ls ks (((destino x y [l])!!0 - (destino x y [k])!!0):((destino x y [l])!!1 - (destino x y [k])!!1):[])   = True
                                     | otherwise = False
inferiorAux n [] [] [x,y] = True

