import System.IO()
import Data.Time
import Data.List
import Control.Exception

-- Tipo alias para la placa del vehículo
type Plate = String

-- Tipo para representar un vehículo con su placa y tiempo de entrada
data Vehicle = Vehicle { plate :: Plate, entryTime :: UTCTime } deriving (Show, Read)

-- Tipo para representar un registro de entrada o salida
data Record = Entry Vehicle | Exit Plate UTCTime deriving (Show, Read)

-- Función principal: carga los registros y muestra el menú principal
main :: IO ()
main = do
  vehicles <- loadVehicles "Parqueadero.txt" -- Cargar registros desde el archivo
  menu vehicles -- Mostrar el menú principal

-- Función para mostrar el menú principal
menu :: [Record] -> IO ()
menu vehicles = do
  putStrLn "Sistema de Gestión de Estacionamiento"
  putStrLn "1. Registrar entrada de vehículo"
  putStrLn "2. Registrar salida de vehículo"
  putStrLn "3. Buscar vehículo por placa"
  putStrLn "4. Listar vehículos"
  putStrLn "5. Salir"
  putStrLn "Seleccione una opción:"
  option <- getLine
  case option of
    "1" -> checkIn vehicles
    "2" -> checkOut vehicles
    "3" -> searchVehicle vehicles
    "4" -> listVehicles vehicles
    "5" -> saveAndExit vehicles
    _   -> putStrLn "Opción no válida" >> menu vehicles

-- Función para registrar la entrada de un vehículo
checkIn :: [Record] -> IO ()
checkIn vehicles = do
  putStrLn "Ingrese la placa del vehículo:"
  plateInput <- getLine
  currentTime <- getCurrentTime -- Obtener la hora actual
  if any (\r -> case r of
                  Entry v -> plateInput == plate v -- Verificar si la placa ya está registrada
                  _       -> False) vehicles
    then putStrLn "El vehículo ya está registrado en el estacionamiento" >> menu vehicles
    else do
      let vehicle = Vehicle plateInput currentTime -- Crear un nuevo registro de vehículo
      let updatedVehicles = vehicles ++ [Entry vehicle] -- Agregar el registro a la lista
      putStrLn "Vehículo registrado exitosamente"
      menu updatedVehicles -- Volver al menú principal

-- Función para registrar la salida de un vehículo
checkOut :: [Record] -> IO ()
checkOut vehicles = do
  putStrLn "Ingrese la placa del vehículo:"
  plateInput <- getLine
  currentTime <- getCurrentTime -- Obtener la hora actual
  let updatedVehicles = deleteByPlate plateInput vehicles -- Eliminar el registro de entrada
  case find (\r -> case r of
                     Entry v -> plateInput == plate v -- Buscar el vehículo por su placa
                     _       -> False) vehicles of
    Just (Entry vehicle) -> do
      let timeDiff = diffUTCTime currentTime (entryTime vehicle) -- Calcular el tiempo de permanencia
      putStrLn $ "Tiempo en el estacionamiento: " ++ show timeDiff
      menu (updatedVehicles ++ [Exit plateInput currentTime]) -- Registrar la salida y volver al menú
    _ -> putStrLn "El vehículo no está en el estacionamiento" >> menu vehicles

-- Función para buscar un vehículo por su placa
searchVehicle :: [Record] -> IO ()
searchVehicle vehicles = do
  putStrLn "Ingrese la placa del vehículo:"
  plateInput <- getLine
  case find (\r -> case r of
                     Entry v -> plateInput == plate v -- Buscar el vehículo por su placa
                     _       -> False) vehicles of
    Just (Entry vehicle) -> do
      putStrLn $ "Vehículo encontrado: " ++ show vehicle
      menu vehicles -- Volver al menú principal
    _ -> putStrLn "El vehículo no está en el estacionamiento" >> menu vehicles

-- Función para listar todos los vehículos en el estacionamiento
listVehicles :: [Record] -> IO ()
listVehicles vehicles = do
  putStrLn "Lista de vehículos en el estacionamiento:"
  mapM_ print [v | Entry v <- vehicles] -- Imprimir todos los registros de entrada
  menu vehicles -- Volver al menú principal

-- Función para guardar los registros y salir del programa
saveAndExit :: [Record] -> IO ()
saveAndExit vehicles = do
  saveVehicles "Parqueadero.txt" vehicles -- Guardar los registros en el archivo
  putStrLn "Guardando y saliendo..."
  return ()

-- Función para cargar los registros desde un archivo
loadVehicles :: FilePath -> IO [Record]
loadVehicles path = do
  catch (read <$> readFile path) handleError -- Leer los registros del archivo
  where
    handleError :: IOError -> IO [Record]
    handleError _ = return [] -- Manejar errores y devolver una lista vacía

-- Función para guardar los registros en un archivo
saveVehicles :: FilePath -> [Record] -> IO ()
saveVehicles path vehicles = writeFile path (show vehicles) -- Guardar los registros en el archivo

-- Función auxiliar para eliminar un registro de entrada por la placa
deleteByPlate :: Plate -> [Record] -> [Record]
deleteByPlate plateInput = filter (\r -> case r of
                                           Entry v -> plateInput /= plate v -- Filtrar el registro por la placa
                                           _       -> True)
