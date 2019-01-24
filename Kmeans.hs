module Kmeans
    (
     kmedias,
     classifyUno,
     classifyLista
        ) where

import Data.List
import Data.Ord


-------------------FUNCIONES AUXILIARES------------------------

--definición de distancia euclidea
euclidea :: [Double] -> [Double] -> Double
euclidea xs ys = sqrt (sum [((a-b)*(a-b)) | (a,b) <- zip xs ys])

--funcion que calcula la media aritmética de una lista

--      IMPORTANTE: En algunas ocasiones, no se clasificará ningún
--      valor para un centro en concreto. Es por eso que acabará
--      dividiéndose entre 0, y el algoritmo devolverá valores NaN.
--      Por ello, se ha añadido que se la longitud de la lista de
--      valores clasificados para ese punto es 0, se devuelva 0.
mediaAritmetica :: (Foldable t, Fractional t1) => t t1 -> t1
mediaAritmetica xs 
    |length xs == 0 = 0.0
    |otherwise = sum xs / fromIntegral (length xs)

--funcion para acceder a una tupla
prim :: (a,b) -> a
prim (a,_) = a
seg :: (a,b) -> b
seg (_,b) = b

--funcion para hacer la inversa de una lista
inversa :: [a] -> [a]
inversa xs = foldr (\x y -> y ++ [x]) [] xs

---------------------------------------------------------------




---------------------DATOS AUXILIARES--------------------------
-- nA::Int
-- nA = 2
-- nC = 7
-- entrenamiento = [[51.0,40.0],[43.0,52.5],[62.0,71.9],[64.0,65.0],[45.0,90.0],
--                [42.0,45.0],[46.0,45.0],[45.0,46.0],[45.0,46.0],
--                [62.0,63.0],[47.0,56.0],[52.0,60.0],[64.0,65.0],
--                [51.0,20.0],[65.0,70.0],[48.0,50.0],[49.0,70.0],
--                [46.0,52.0],[64.0,65.0],[51.0,49.0],[52.0,67.0],
--                [62.0,54.0],[49.0,60.0],[48.0,50.0],[62.0,67.0]]

---------------------------------------------------------------




----------------------ALGORITMO--------------------------------



-- Esta función devuelve una lista de centros según el número
-- de clusters que queramos: si queremos 3 clusters, nos
-- devuelve una lista con tres listas, cada una de estas
-- representa un centro


centrosInic :: (Num t, Eq t) => [t1] -> t -> [t1]
centrosInic (x:datos) 0 = []
centrosInic (x:datos) nC = x:(centrosInic datos (nC-1))



-- Se escogen los centros iniciales (uno por cada cluster)
-- Para eso se usa la funcion "nearestCenter" que calcula, de
-- la lista de centros, cuál es el más cercano. La función 
-- devuelve una lista con tuplas, indicando qué cluster está
-- más cerca de cada dato (desde 1 hasta el numero de Clusters):

asignaCluster::(Ord a, Enum t, Num t) =>[[t2]] -> [t1] -> ([t2] -> t1 -> a) -> t -> [([t2], t)]
asignaCluster [] lcent fdist _ = []
asignaCluster (x:datos) lcent fdist nC = (x,(nearestCenter x lcent fdist nC)):(asignaCluster datos lcent fdist nC)


nearestCenter::(Num t, Enum t, Ord a) =>[t2] -> [t1] -> ([t2] -> t1 -> a) -> t -> t
nearestCenter dato lcent fdist nC = sum [y |(x,y)<- disClus, x==minim]
        where minim = minimum [(fst x)| x<-disClus]
              disClus = [((fdist [dato!!0] y),n)| (y,n) <- zip lcent [1..nC]]



-- Recalcula los centros:
--  1. procesaCentros: esta función devuelve una lista con tantas
--      listas como número de clusters haya: en cada lista se
--      encuentra la lista de puntos que se ha clasificado para
--      cada cluster: en la primera lista se encuentran todos los
--      puntos clasificados para el cluster 1, en la segunda aquellos
--      clasificados para el cluster 2, y así.
--          Nota: Debido al contador, los centros saldrán en orden
--          inverso. Por ello, al final, le haremos la inversa
--          a la lista de centros obtenidos.

procesaCentros :: (Eq t, Num t) => [(t1, t)] -> t -> [[t1]]
procesaCentros clasif 0 = [] 
procesaCentros clasif cont = 
    [(prim x) | x <- clasif, (seg x) == cont]:procesaCentros clasif (cont-1)


-- Recalcula los centros:
--  2. calculaCentro: esta función coge una lista de puntos y hace
--      la media aritmética de los elementos según su posición:
--      hace la media aritmética de todos los elementos que ocupan
--      la posición 1, de todos los que ocupan la posición 2...
--      Devuelve una lista con estas medias calculadas

calculaCentro :: Fractional t => [[t]] -> Int -> Int -> [t]
calculaCentro xs n nClus
    |n == nClus = []
    |otherwise = (mediaAritmetica [x !! n| x<- xs]):calculaCentro xs (n+1) nClus


-- Recalcula los centros:
--  3. nuevosCentros: calcula los centros de manera recursiva:
--      primero calcula el nuevo centro del cluster 1 y en la
--      siguiente llamada recursiva, los del cluster 2... y así.

nuevosCentros :: Fractional t => [[[t]]] -> Int -> [[t]]
nuevosCentros [] _ = []
nuevosCentros (x:xs) numAtr = (calculaCentro x 0 numAtr):nuevosCentros xs numAtr

--------------------------------------------------------------------




-------------------FUNCIONES VISIBLES DEL MÓDULO--------------------


-- Función para la clasificación de un punto: a esta función se le llama
-- diciendo: Kmeans.classify. Hace como un kNN - 1
--classifyUno::(Enum t, Num t, Eq t) => [[Double]] -> t -> Int -> [Double] -> t
classifyUno entrenamiento nCl nA nuevoValor = do

    let nC = nCl
    let res = nearestCenter nuevoValor cent euclidea nCl
    (nuevoValor, res, asignaCentro res cent)
        where cent = algoritmo centros centros entrenamiento nCl nA 1 1000 
              centros = centrosInic entrenamiento nCl


-- Función que clasifica una lista entera de puntos. Llama a la función
-- classifyUno de forma recursiva hasta que se han agotado los puntos.
--classifyLista::(Eq t, Num t, Enum t) => [[Double]] -> t -> Int -> [[Double]] -> [([Double], t)]
classifyLista entrenamiento nCl nA [] = []
classifyLista entrenamiento nCl nA (x:valores) = 
    classifyUno entrenamiento nCl nA x: classifyLista entrenamiento nCl nA valores


-- Función para el entrenamiento de los clusters: a esta función se le
-- llama diciendo: Kmeans.kmedias
kmedias::(Eq t, Enum t, Num t) => [[Double]] -> t -> Int -> [[Double]]
kmedias entrenamiento nCl nA = 
    algoritmo centros centros entrenamiento nCl nA 1 1000
        where centros = centrosInic entrenamiento nCl 


-- Función auxiliar que ejecuta secuencialmente todos los pasos del
-- algoritmo:
--  1. Si se han hecho 1000 iteraciones, se devuelven los centros
--  2. Si la distancia entre los centros de la iteración anterior y los
--      de la nueva es menor que 0.0001; se devuelven los centros
--  3. Si no ocurre ni 1 ni 2, hay que seguir haciendo iteraciones:
--      3.1 - Se calcula la distancia euclidea desde cada atributo a los
--              centros, y a la menor distancia, se le asigna el número
--              del cluster al que pertenece este centro.
--      3.2 - En una lista, se incluyen listas que contengan los elementos
--              clasificados: [[elementos clasf cluster 1],[elementos clas
--              cluster 2], ..., [elementos clasf cluster n]]
--      3.3 - Se hace la media aritmética de cada lista perteneciente a esa
--              lista, y este valor será el nuevo centro. NOTA: no se hace
--              la media de todos los valores a la vez, si no de cada uno
--              por separado: los primeros con los primeros, los segundos
--              con los segundos, etc.
--      3,4 - Se hace la inversa de los centros obtenidos: debido a que la
--              función procesaCentros va con un contador desde nC -> 0, los
--              resultados salen al revés.

algoritmo::(Num t, Num t1, Enum t1, Eq t, Eq t1)=>[[Double]]->[[Double]]->[[Double]]-> t1-> Int-> Double-> t-> [[Double]]
algoritmo centrosAntiguos cents entrenamiento nCl nA umbri cont
    |cont == 0 = cents
    |umbri < 0.0001 = cents
    |otherwise = algoritmo cents centrosNuevos entrenamiento nCl nA umbr (cont-1)
        where clasificacion = asignaCluster entrenamiento cents euclidea nCl
              nuevos = (nuevosCentros (procesaCentros clasificacion nCl) nA)
              centrosNuevos = inversa nuevos
              umbr = umbral centrosAntiguos centrosNuevos


umbral :: [[Double]] -> [[Double]] -> Double
umbral xs ys = sum [euclidea x y|(x,y) <- zip xs ys]

asignaCentro numClus centroids = centroids !! (numClus-1)



---------------------------------------------------------------



