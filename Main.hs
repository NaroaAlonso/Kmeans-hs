import Text.CSV
import System.Environment (getArgs)
import Data.Ord (comparing)
import Data.List
import Kmeans

main :: IO ()
main = do



---------------------------------------------------------------
---------------------------------------------------------------
--      LECTURA DEL FICHERO DE DATOS PARA HACER KMEANS
---------------------------------------------------------------
---------------------------------------------------------------

-- Nombre del fichero por definicion: data/iris.csv
  --ruta <- getArgs
  --let pathArchivo = if null ruta then "data/iris.csv" else ruta!!0
  putStrLn $ "Indique el fichero para conjunto de entrenamiento. Si no se indica nada, será data/iris.csv"
  ruta <- getLine
  let pathArchivo = if null ruta then "data/iris.csv" else ruta



-- Leer contenido de fichero
  input <- readFile pathArchivo


-- ¿Es un CSV válido?
  let datos = parseCSV pathArchivo input


--  Para hacerlo lo más general posible, preguntamos por el núm
--  de atributos que contiene el fichero, y filtramos las filas
--  válidas a través de este número.
  let inputatr = "Indique el número de atributos"     
  putStrLn inputatr
  nA <- getLine
  let numAtr = read nA::Int


-- csv es de tipo Either Text.Parsec.Error.ParseError CSV 
-- Si no hay error, devuelve (Right x), con x de tipo CSV
-- Tratamos estos dos casos:
      filas = case datos of
        
        -- Devolvemos la lista de líneas del archivo si devuelve algo de tipo CSV
        (Right lineas) -> lineas
        -- En caso contrario (_, en este caso (Left y)), devolvemos []
        _ -> []
      

-- el fichero con el que se trabaja tiene campos = 
-- número de atributos, así que filtramos las líneas
      filasNombreAtr = filter (\x -> length x == numAtr) filas
      

      --Ahora, para las filas seleccionadas: si no hay, devolvemos nombre y atributoss vacios
      --Si hay, devolvemos el par nombre atributos
      (nombre,atributos) = case filasNombreAtr of

        [] -> ([],[])
        (nom:atr) -> (nom,atr)


  if null filasNombreAtr then
    putStrLn $ printCSV filas




-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
--  EN CASO DE QUE LA LECTURA HAYA IDO BIEN
--  Inserción de parámetros y mostrar por pantalla qué hay
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


-- Si tenemos filas validas, imprimimos por pantalla:
-- 1. el número de campos que hay en el archivo (en el caso ejemplo: iris.csv tenemos 5 campos)
-- 2. la lista de los campos (longitud y profundidad sepalo, long y prof del petalo y clase)
-- 3. Si no hay datos disponibles, imprimiremos "No hay datos dispobnibles en el fichero"
-- 4. Guardamos esta lista con el nombre "conjuntoEntrenamiento"

  else do
    putStrLn $ "Número de campos: " ++ show (length nombre)
    putStrLn $ "Lista de campos: \n\t" ++ "\t{" ++ (init.init.concat) ([ c ++ ", " | c <- nombre ]) ++ "}"

    if null atributos then
      putStrLn "No hay datos disponibles en el fichero"
    
    else do
      putStrLn $ "Número de ejemplos: " ++ show (length atributos)



-- Preguntamos por el número de clusters (Ya que es un parámetro
-- de entrada en el algoritmo de kmedias):
      let inputclusters = "Indique el número de clusters"
      putStrLn inputclusters
      nC <- getLine
-- Pasamos el número de clusters a tipo Int, para poder
-- pasarle el algoritmo
      let clusters = read nC::Int
      

-- Mostramos los datos que tenemos por pantalla:
      putStrLn $ "Número de clusters: " ++ show (clusters) ++ "\nNúmero de atributos: " ++ show (numAtr)


-- Como vamos a tener que usar el conjunto de entrenamiento en
-- cualquier parte del menú, lo procesamos antes de entrar,
-- de este modo no tenemos que incluir este código 3 veces 
-- más adelante
      let conjuntoEntrenamiento = guardaConjunto atributos
      let entrenamiento = pasaDouble conjuntoEntrenamiento
        



--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
--  MENÚ
--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------


-- Preguntamos qué queremos:
--  1. Si queremos entrenar un conjunto, pulsamos 1
--  2. Si queremos clasificar un valor, pulsamos 2
--  3. Si queremos clasificar valores de un CSV, pulsamos 3

      let menu = "- Para visualizar los centroides, pulse 1" ++ "\n- Para clasificar un valor con un conjunto, pulse 2" ++ "\n- Para clasificar un conjunto de valores desde un CSV, pulse 3"
      putStrLn $ menu

      choice <- getLine
      let eleccion = read choice::Int



--  Si no se ha pulsado nada, error
      if null choice then
        putStrLn $ "Error: no ha elegido nada"


-----------------------------------------------------------------------------------------------
--      PARTE 1 DEL MENÚ
-----------------------------------------------------------------------------------------------

--  Si se ha pulsado 1: se lleva a cabo kmedias. Luego, se muestra
--  por pantalla, y si queremos, podemos escribirlo en un fichero
--  aparte. Si no, se acaba el programa

      else if eleccion == 1 then do
        
        let centroids = Kmeans.kmedias entrenamiento clusters numAtr
        let resultado = procesaCentros centroids clusters

        putStrLn "\n ********************************************* \n  Procesando...\n *********************************************\n "
        sequence_ (map putStrLn resultado)



        putStrLn $ "Si desea guardarlo en un fichero, pulse 1. Si no, pulse cualquier otro número"

        ch<- getLine
        let el = read ch::Int

        if el==1 then do
            
            putStrLn $ "Escriba el nombre del fichero. Si no indica nada, el fichero se llamará centroides.txt"
            nombreFich<- getLine
            let camin = if null nombreFich then "centroides.txt" else nombreFich
            writeFile camin (show (resultado))
            putStrLn $ "Gracias por usar el programa."
        
        else do
            putStrLn $ "OK. Bye"


-----------------------------------------------------------------------------------------------
--      PARTE 2 DEL MENÚ
-----------------------------------------------------------------------------------------------

--  Si se ha pulsado 2: se lleva a cabo la clasificación de un
--  valor únicamente, y esta se muestra por pantalla

      else if eleccion == 2 then do

        let preguntaValor = "Ahora, introduzca el valor a clasificar. Ejemplo: [1.0, 2.2, 3.0]"
        putStrLn preguntaValor
        val <- getLine

        let valor = read val::[Double]
        putStrLn "\n ********************************************* \n  Procesando...\n *********************************************\n "

        let clasif = Kmeans.classifyUno entrenamiento clusters numAtr valor
        let clasifi = procesaRes2 clasif
        formato clasifi
      
        --putStrLn $ "\nEl cluster para el que ha sido clasificado es el número " ++ show (clasif) 


-----------------------------------------------------------------------------------------------
--      PARTE 3 DEL MENÚ
-----------------------------------------------------------------------------------------------

--  Si se ha elegido el 3: primero se pregunta por el fichero.
--  Luego, se lee el fichero y se van clasificando los valores
--  Uno por uno. Tras esto, se escribe el resultado en un fichero

      else if eleccion == 3  then do

        --LECTURA DEL FICHERO
        
        putStrLn $ "Por favor, indique la ruta del archivo con el conjunto de prueba: "
        nombreFichero <- getLine

        let path = if null nombreFichero then "data/conjuntoPruebaIris.csv" else nombreFichero
        input <- readFile path

        let csv = parseCSV path input
        let filas = case csv of
                (Right lineas) -> lineas
                (Left err) -> []
        
        let filasValidas = filter (\x -> length x == numAtr) filas

            (cabecera,registros) = case filasValidas of
                [] -> ([],[])
                (cab:regs) -> (cab,regs)
        
        if null filasValidas then
            putStrLn "El fichero no es un CSV Válido"

        else do
            putStrLn $ "    - Número de atributos: " ++ show (length cabecera)
            putStrLn $ "    - Nombres de los atributos: " ++ "{" ++ (init.init.concat) ([ c ++ ", " | c <- cabecera]) ++ "}"
            
            if null registros then
                putStrLn "No hay puntos para clasificar"

            else do
                putStrLn $ "    - Puntos a clasificar: " ++ show (length registros)
                putStrLn "\n ********************************************* \n  Procesando...\n *********************************************\n "
                --sequence_ [muestraReg reg cabecera | reg <- registros]
                    --where muestraReg reg cab = putStrLn $ "\t{" ++ (init.init.concat) ([cpo ++ ": " ++ val ++ ", " | (cpo,val) <- zip cab reg]) ++"}"
                let valAClasif = guardaConjunto registros
                let valores = pasaDouble valAClasif
                let fin = Kmeans.classifyLista entrenamiento clusters numAtr valores
                
                -- procesamos el resultado para obtener qué es cada cosa que devuelve la tupla
                -- y lo mostramos por pantalla con el formato correspondiente
                let final = procesaRes3 fin
                formato final

                --writeFile "clasif.txt" (show (final))
                --archivo <- readFile "clasif.txt" 

                --Preguntamos si quieren escribir el resultado en otro fichero:
                putStrLn $ "    ¿Desea exportar los resultados a un fichero? Pulse 1 para proceder o pulse cualquier otro número para finalizar el programa"
                a <- getLine
                let cha = read a::Int

                if cha == 1 then do
                    putStrLn $ "Escriba el nombre del fichero en el que quiere exportar los datos. Si no escribe nada, el fichero se llamará clasificacion.txt"
                    nombreFicher<- getLine
                    let cam = if null nombreFicher then "data/clasificacion.txt" else nombreFicher
                    let aEscribir = lines (show (final))
                    writeFile cam (show (aEscribir))
                    putStrLn $ "Gracias por usar el programa."

        
                else do
                    putStrLn $ "OK. Bye"

--  Si no se ha pulsado ni 1 ni 2 ni 3, se comunica.
      else do
        putStrLn $ "ERROR. No ha pulsado ninguna de las opciones disponibles"


      

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
--          FUNCIONES AUXILIARES LECTURA FICHEROS
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-- Guarda los valores leidos desde el fichero en un conjunto
guardaConjunto [] = []
guardaConjunto elementos = [elem | elem <- elementos]


-- Pasa los elementos de una lista que se ha leido a Double
pasaDouble [] = []
pasaDouble (x:xs) = ([read reg::Double| reg<- x]): pasaDouble xs


-- Para la opción 1 del menú, cuando enseñe los centroides por pantalla
-- lo hará con este formato: "CENTROIDE X", [centroide]
procesaCentros centroides nC = [("-----------------------------------------\nCENTROIDE " ++show (i+1) ++": "++ show (centroides !! i))| i<- [0..(nC-1)]]

-- Para la opción 3 del menú, cuando enseñe los resultados por pantalla
-- lo hará con este formato:

--------------------------------------
-- PUNTO A CLASIFICAR: [punto]
-- CLUSTER ASIGNADO: número de Cluster
-- CENTROIDE DEL CLUSTER: [centroide]
--------------------------------------

procesaRes3 tupla = [("\nPUNTO A CLASIFICAR: "++show (prim x)++"\nCLUSTER ASIGNADO: "++show(seg x)++"\nCENTROIDE DEL CLUSTER: "++show(ter x)++"\n\n-----------------------------------------") | x<- tupla]
formato lista = sequence_ (map putStrLn lista)

procesaRes2 tupla = [("\nPUNTO A CLASIFICAR: "++show (prim tupla)++"\nCLUSTER ASIGNADO: "++show(seg tupla)++"\nCENTROIDE DEL CLUSTER: "++show(ter tupla)++"\n\n-----------------------------------------")]


---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
--      FUNCIONES AUXILIARES ALGORITMO / FORMATO
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

--funcion para acceder a una tupla
prim :: (a,b,c) -> a
prim (a,_,_) = a
seg :: (a,b,c) -> b
seg (_,b,_) = b
ter :: (a,b,c) -> c
ter (_,_,c) = c




















