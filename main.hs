import System.Random
import System.IO
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Control.DeepSeq
import Data.Time.Clock
import Data.List
import Text.Printf

--Grupo :: Daniel Díaz - Nicol Huaiquil - Gustavo Reyes - Andrés Torres

data QTree = Leaf | QNode [(Int,Int)] Int Double Double Double QTree QTree QTree QTree deriving (Show)

--getters para los datos del nodo
getData :: QTree -> [(Int,Int)]
getData (QNode a _ _ _ _ _ _ _ _) = a
getData Leaf = []

getParticleLimit :: QTree -> Int
getParticleLimit (QNode _ b _ _ _ _ _ _ _) = b
getParticleLimit Leaf = 0 

getRegionGrid :: QTree -> Double
getRegionGrid (QNode _ _ c _ _ _ _ _ _) = c
getRegionGrid Leaf = 0 

getBoundX :: QTree -> Double
getBoundX (QNode _ _ _ x _ _ _ _ _) = x
getBoundX Leaf = 0

getBoundY :: QTree -> Double
getBoundY (QNode _ _ _ _ y _ _ _ _) = y
getBoundY Leaf = 0

--construir raiz
buildQt :: [(Int,Int)] -> Int -> Double -> QTree
buildQt arr q l 
  | length arr > q = QNode arr q l 0 0 (buildQt2 arr q l1 root 0 0) (buildQt2 arr q l1 root l1 0) (buildQt2 arr q l1 root 0 l1) (buildQt2 arr q l1 root l1 l1)
  | otherwise = root
  where l1 = l/2
        root = QNode arr q l 0 0 Leaf Leaf Leaf Leaf

--construir resto del arbol segun las reglas del enunciado: Si hay mas particulas de las aceptadas, segmentar/En otro caso, añadir una hoja.		
buildQt2 :: [(Int,Int)] -> Int -> Double -> QTree -> Double -> Double -> QTree
buildQt2 arr q l nodo x y
  | length corte > q = QNode corte q l x y (buildQt2 corte q l1 node x y) (buildQt2 corte q l1 node (x+l1) y) (buildQt2 corte q l1 node x (y+l1)) (buildQt2 corte q l1 node (x+l1) (y+l1))
  | otherwise = node
  where corte = getParticlesInBounds arr l x y ((length arr) - 1)
        l1 = l/2
        node = QNode corte q l x y Leaf Leaf Leaf Leaf

--Construye la lista de particulas en la region para cada nodo
getParticlesInBounds :: [(Int,Int)] -> Double -> Double -> Double -> Int -> [(Int,Int)]
getParticlesInBounds arr l x y i 
  | i == -1 = []
  | checkX && checkY = [arr !! i] ++ getParticlesInBounds arr l x y (i-1)
  | otherwise = [] ++ getParticlesInBounds arr l x y (i-1)
  where checkX = (floor x <= arrX) && (arrX <= ceiling (x + l))
        checkY = (floor y <= arrY) && (arrY <= ceiling (y + l))
        arrX = fromIntegral (fst (arr !! i))
        arrY = fromIntegral (snd (arr !! i))

--verifica si existe colision de la particula recorriendo el arbol. Usa funciones auxiliares checkParticlesIsInBounds y colisionNodo.
checkColision :: (Int,Int) -> Int -> QTree -> [(Int,Int)]
checkColision pr r (QNode arr q l x y Leaf Leaf Leaf Leaf) = delete pr (colisionNodo arr pr r ((length arr) -1))
checkColision pr r (QNode arr q l x y n1 n2 n3 n4) = (if condicion1 then task1 else [])++(if condicion2 then task2 else [])++(if condicion3 then task3 else [])++(if condicion4 then task4 else [])
  where
   condicion1 = checkParticleIsInBounds n1 pr
   condicion2 = checkParticleIsInBounds n2 pr
   condicion3 = checkParticleIsInBounds n3 pr
   condicion4 = checkParticleIsInBounds n4 pr
   task1 = checkColision pr r n1
   task2 = checkColision pr r n2
   task3 = checkColision pr r n3
   task4 = checkColision pr r n4

--revisa si la particula pertenece a la region
checkParticleIsInBounds :: QTree -> (Int, Int) -> Bool
checkParticleIsInBounds node pr = checkX && checkY
  where checkX = (floor boundX <= prX) && (prX <= ceiling (boundX + gridL))
        checkY = (floor boundY <= prY) && (prY <= ceiling (boundY + gridL))
        boundX = getBoundX node
        boundY = getBoundY node
        gridL = getRegionGrid node
        prX = fromIntegral (fst pr)
        prY = fromIntegral (snd pr)

--revisa la colision entre las particulas pertenecientes de una respectiva region.
colisionNodo :: [(Int, Int)] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
colisionNodo arr pr r n
  | n == -1 = []
  | dist < r2 = [arr !! n] ++ colisionNodo arr pr r (n-1)
  | otherwise = [] ++ colisionNodo arr pr r (n-1)
  where dx = fromIntegral (fst pr - fst (arr !! n))
        dy = fromIntegral (snd pr - snd (arr !! n))
        calc = (dx*dx) + (dy*dy)
        dist :: Double
        dist = sqrt calc
        r2 = fromIntegral (r*2)

colQT :: Int -> Int -> QTree -> Int -> [(Int,Int)] -> [[(Int,Int)]]
colQT i n nodo r arr
  | i == n+1 = []
  | otherwise = [checkColision (arr !! i) r nodo] ++ colQT (i+1) n nodo r arr
  
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "listo en: %.3fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  
buildArray :: [Int] -> [Int] -> [(Int,Int)]
buildArray [] _ = []
buildArray a b = [(head a, head b)] ++ buildArray (tail a) (tail b)

buildParticles :: Int -> Double -> StdGen -> StdGen -> [(Int,Int)]
buildParticles n l s1 s2 = let x = take n $ randomRs (0,floor l) s1 :: [Int]
                               y = take n $ randomRs (0,floor l) s2 :: [Int]
                           in buildArray x y

colisionBruta :: [(Int, Int)] -> Int -> Int -> Int -> [(Int, Int)]
colisionBruta arr i n r 
  | n == -1 = []
  | i == n = [] ++ colisionBruta arr i (n-1) r
  | sqrtn < r2 = [arr !! n] ++ colisionBruta arr i (n-1) r
  | otherwise = [] ++ colisionBruta arr i (n-1) r
  where dx = fromIntegral (fst (arr !! i) - fst (arr !! n))
        dy = fromIntegral (snd (arr !! i) - snd (arr !! n))
        dist = (dx*dx) + (dy*dy)
        sqrtn :: Double
        sqrtn = sqrt dist
        r2 = fromIntegral (r*2)

colFB ::  Int -> Int -> Int -> [(Int,Int)] -> [[(Int,Int)]]
colFB i n r arr 
  | i == n+1 = []
  | otherwise = [colisionBruta arr i n r] ++ colFB (i+1) n r arr

main = do
  args <- getArgs
  if (length args) /= 5
      then error $ "run as ./prog m r l q n\nm = metodo\nr = radio\nl = tamaño del espacio\nq = maximo de particulas por region\nn = numero de particulas"
      else return ()
  let m = read (args !! 0) :: Int
  let r = read (args !! 1) :: Int
  let l = read (args !! 2) :: Double
  let q = read (args !! 3) :: Int
  let n = read (args !! 4) :: Int
 
  printf "---==<:[ Deteccion de colisiones ]:>==---\n"
  printf "metodo = %i " m
  case m of
    0 -> printf "---> Fuerza bruta\n"
    1 -> printf "---> QuadTree\n"
    _ -> error $ "METODO INVALIDO, use m = 0 o m = 1"
  printf "r = %i " r
  printf "l = %f " l
  printf "q = %i " q
  printf "n = %i\n" n
  sd1 <- newStdGen
  sd2 <- newStdGen
  
  printf ("Construyendo particulas....................")
  t0 <- getCurrentTime
  particles <- evaluate (buildParticles n l sd1 sd2 `using` rdeepseq)
  printTimeSince t0
  
  arbolote <- evaluate (buildQt particles q l `using` rseq)

  printf ("Calculando colisiones....................")
  t0 <- getCurrentTime
  let task = [colFB 0 (n-1) r, colQT 0 (n-1) arbolote r] !! m
  d <- evaluate (task particles `using` parListChunk 500 rdeepseq)
  printTimeSince t0
  if n <= 200
      then printf (printColisiones particles d (-1))
      else printf "La cosa funciona, solo que el n es muy grande\n"


printColisiones :: [(Int,Int)] -> [[(Int,Int)]] -> Int-> String
printColisiones pr col i
  | i == n = "======Fin de la lista======"
  | i == -1 = "====Lista de colisiones====\n" ++ printColisiones pr col (i+1)
  | (col !! i) == [] = "" ++ printColisiones pr col (i+1)
  | otherwise = "Punto "++ show (pr !! i) ++ "\t:  " ++ noBracketsList ++ "\n" ++ printColisiones pr col (i+1)
  where noBracketsList = filter (not . (`elem` "[]")) (show (col !! i))
        n = length pr

