--Ignacio de la Cruz
--1
data Direc = Arriba | Abajo | Derecha | Izquierda deriving (Enum, Show, Eq, Ord)
destino :: Num a => a -> a -> [Direc] -> [a]
destino x y [] = [x,y]
destino x y (l:ls) | l == Arriba =  destino x (y+1) ls
                   | l == Abajo = destino x (y-1) ls
                   | l == Derecha = destino (x+1) y ls
                   | l == Izquierda = destino (x-1) y ls
 
--2

data Nat = Cero | Suc Nat deriving (Eq,Ord)
infixl 4 +++
(+++) :: Nat -> Nat -> Nat
x +++ Cero = x
x +++ (Suc y) = Suc x +++ y
infixl 6 ***
(***) :: Nat -> Nat -> Nat
x *** Cero = Cero
x *** (Suc y) = Suc x *** y
natToInt :: (Num t, Enum t) => Nat -> t
natToInt Cero = 0
natToInt (Suc x) = succ (natToInt x)
instance Show Nat where
 show x = show (natToInt x)
--3
data NumComplejo = NumComplejo Float Float deriving Show
instance Num NumComplejo where
 NumComplejo x y * NumComplejo z k = NumComplejo (x*z - y*k) (x*k +y*z)
 NumComplejo x y + NumComplejo z k = NumComplejo (x+z) (y+k)
 NumComplejo x y - NumComplejo z k = NumComplejo (x-z) (y-k)
 
--4
--medida :: Medible a => a -> Int

class Medible a where
 medida::a->Int
 
instance Medible Bool where
 medida x | x==True = 0
          | x==False = 1
instance Medible [a] where
 
 medida [] = 0
 medida (x:xs) = 1 + medida xs
instance Medible (a,b) where
 medida (x,y) = 100 
