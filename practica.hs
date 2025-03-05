import Data.Time.Clock
import Data.List
import System.IO
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    ingreso :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el estudiante aún está en la universidad o ya salió
} deriving (Show, Read)

-- Función para registrar el ingreso de un estudiante a la universidad
registrarIngreso :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarIngreso idEst tiempo estudiantes =
    Estudiante idEst tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante de la universidad
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idEst tiempo estudiantes =
    map (\e -> if idEst == idEstudiante e then e { salida = Just tiempo } else e) estudiantes

-- Función para buscar un estudiante por su ID en la universidad (solo si aún se encuentra dentro)
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idEst estudiantes =
    find (\e -> idEst == idEstudiante e && isNothing (salida e)) estudiantes
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un estudiante ha permanecido en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (ingreso estudiante)

-- Función para guardar la información de los estudiantes en un archivo de texto
guardarEstudiantes :: [Estudiante] -> IO ()
guardarEstudiantes estudiantes = do
    withFile "University.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante estudiantes))
    putStrLn "Información de estudiantes guardada en el archivo University.txt."

-- Función para cargar la información de los estudiantes desde un archivo de texto
cargarEstudiantes :: IO [Estudiante]
cargarEstudiantes = do
    contenido <- withFile "University.txt" ReadMode $ \h -> do
        cont <- hGetContents h
        cont `deepseq` return cont
    let lineas = lines contenido
    return (map leerEstudiante lineas)
    where 
        leerEstudiante linea = read linea :: Estudiante

-- Función para mostrar la información de un estudiante como cadena de texto
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante idEst ingreso salida) =
    "Estudiante {idEstudiante = \"" ++ idEst ++ "\", ingreso = " ++ show ingreso ++ ", salida = " ++ maybe "Nothing" show salida ++ "}"

-- Función para listar los estudiantes registrados en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes registrados en la universidad."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ (putStrLn . mostrarEstudiante) estudiantes

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar la información de estudiantes desde el archivo de texto
    estudiantes <- cargarEstudiantes
    putStrLn "¡Bienvenido al Sistema de Gestión de Registro de Estudiantes!"

    -- Ciclo principal del programa
    cicloPrincipal estudiantes

-- Función para el ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar ingreso de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarIngreso idEst tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ idEst ++ " ingresado a la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "2" -> do
            putStrLn "Ingrese el ID del estudiante para registrar su salida:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarSalida idEst tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ idEst ++ " ha salido de la universidad."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst estudiantes of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEst ++ " se encuentra en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado en la universidad."
            cicloPrincipal estudiantes

        "4" -> do
            listarEstudiantes estudiantes
            cicloPrincipal estudiantes

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal estudiantes
