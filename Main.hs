import System.IO()
import Data.Time
import Data.List
import Control.Exception

-- Tipo alias para la placa del vehículo
type Placa = String

-- Tipo para representar un vehículo con su placa y tiempo de entrada
data Vehiculo = Vehiculo { placa :: Placa, horaEntrada :: UTCTime } deriving (Show, Read)

-- Tipo para representar un registro de entrada o salida
data Registro = Entrada Vehiculo | Salida Placa UTCTime deriving (Show, Read)

-- Función principal: carga los registros y muestra el menú principal
main :: IO ()
main = do
  vehiculos <- cargarVehiculos "Parqueadero.txt" -- Cargar registros desde el archivo
  menu vehiculos -- Mostrar el menú principal

-- Función para mostrar el menú principal
menu :: [Registro] -> IO ()
menu vehiculos = do
  putStrLn "Sistema de Gestión de Estacionamiento"
  putStrLn "1. Registrar entrada de vehículo"
  putStrLn "2. Registrar salida de vehículo"
  putStrLn "3. Buscar vehículo por placa"
  putStrLn "4. Listar vehículos"
  putStrLn "5. Salir"
  putStrLn "Seleccione una opción:"
  opcion <- getLine
  case opcion of
    "1" -> registrarEntrada vehiculos
    "2" -> registrarSalida vehiculos
    "3" -> buscarVehiculo vehiculos
    "4" -> listarVehiculos vehiculos
    "5" -> guardarYSalir vehiculos
    _   -> putStrLn "Opción no válida" >> menu vehiculos

-- Función para registrar la entrada de un vehículo
registrarEntrada :: [Registro] -> IO ()
registrarEntrada vehiculos = do
  putStrLn "Ingrese la placa del vehículo:"
  placaInput <- getLine
  horaActual <- getCurrentTime -- Obtener la hora actual
  if any (\r -> case r of
                  Entrada v -> placaInput == placa v -- Verificar si la placa ya está registrada
                  _       -> False) vehiculos
    then putStrLn "El vehículo ya está registrado en el estacionamiento" >> menu vehiculos
    else do
      let vehiculo = Vehiculo placaInput horaActual -- Crear un nuevo registro de vehículo
      let vehiculosActualizados = vehiculos ++ [Entrada vehiculo] -- Agregar el registro a la lista
      putStrLn "Vehículo registrado exitosamente"
      menu vehiculosActualizados -- Volver al menú principal

-- Función para registrar la salida de un vehículo
registrarSalida :: [Registro] -> IO ()
registrarSalida vehiculos = do
  putStrLn "Ingrese la placa del vehículo:"
  placaInput <- getLine
  horaActual <- getCurrentTime -- Obtener la hora actual
  let vehiculosActualizados = eliminarPorPlaca placaInput vehiculos -- Eliminar el registro de entrada
  case find (\r -> case r of
                     Entrada v -> placaInput == placa v -- Buscar el vehículo por su placa
                     _       -> False) vehiculos of
    Just (Entrada vehiculo) -> do
      let diferenciaTiempo = diffUTCTime horaActual (horaEntrada vehiculo) -- Calcular el tiempo de permanencia
      putStrLn $ "Tiempo en el estacionamiento: " ++ show diferenciaTiempo
      menu (vehiculosActualizados ++ [Salida placaInput horaActual]) -- Registrar la salida y volver al menú
    _ -> putStrLn "El vehículo no está en el estacionamiento" >> menu vehiculos

-- Función para buscar un vehículo por su placa
buscarVehiculo :: [Registro] -> IO ()
buscarVehiculo vehiculos = do
  putStrLn "Ingrese la placa del vehículo:"
  placaInput <- getLine
  case find (\r -> case r of
                     Entrada v -> placaInput == placa v -- Buscar el vehículo por su placa
                     _       -> False) vehiculos of
    Just (Entrada vehiculo) -> do
      putStrLn $ "Vehículo encontrado: " ++ show vehiculo
      menu vehiculos -- Volver al menú principal
    _ -> putStrLn "El vehículo no está en el estacionamiento" >> menu vehiculos

-- Función para listar todos los vehículos en el estacionamiento
listarVehiculos :: [Registro] -> IO ()
listarVehiculos vehiculos = do
  putStrLn "Lista de vehículos en el estacionamiento:"
  mapM_ print [v | Entrada v <- vehiculos] -- Imprimir todos los registros de entrada
  menu vehiculos -- Volver al menú principal

-- Función para guardar los registros y salir del programa
guardarYSalir :: [Registro] -> IO ()
guardarYSalir vehiculos = do
  guardarVehiculos "Parqueadero.txt" vehiculos -- Guardar los registros en el archivo
  putStrLn "Guardando y saliendo..."
  return ()

-- Función para cargar los registros desde un archivo
cargarVehiculos :: FilePath -> IO [Registro]
cargarVehiculos ruta = do
  catch (read <$> readFile ruta) manejarError -- Leer los registros del archivo
  where
    manejarError :: IOError -> IO [Registro]
    manejarError _ = return [] -- Manejar errores y devolver una lista vacía

-- Función para guardar los registros en un archivo
guardarVehiculos :: FilePath -> [Registro] -> IO ()
guardarVehiculos ruta vehiculos = writeFile ruta (show vehiculos) -- Guardar los registros en el archivo

-- Función auxiliar para eliminar un registro de entrada por la placa
eliminarPorPlaca :: Placa -> [Registro] -> [Registro]
eliminarPorPlaca placaInput = filter (\r -> case r of
                                           Entrada v -> placaInput /= placa v -- Filtrar el registro por la placa
                                           _       -> True)
