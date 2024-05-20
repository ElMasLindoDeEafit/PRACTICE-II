# PRACTICE-II
ST0244 - Programming Languages Programming Practice II, by Jose Alejandro Jiménez Vasquez
------------------------------------------------------------------------------------------

# Sistema de Gestión de Estacionamiento

Este proyecto fue desarrollado por Jose Alejandro Jimenez utilizando Haskell. El desarrollo se realizó en IntelliJ IDEA principalmente, se depuró y probo replit.com debido a problemas con la IDE de IntelliJ IDEA durante la ejecución. El sistema operativo utilizado fue un kernel de Linux con la distribución de Kali Linux.

## Descripción

El programa permite gestionar el registro de vehículos que entran y salen de un estacionamiento. Permite registrar la entrada y salida de vehículos, buscar vehículos por su placa y calcular el tiempo que un vehículo ha permanecido en el estacionamiento.

## Requisitos Técnicos

- El programa está desarrollado 100% en Haskell.
- El programa permite guardar la información del vehículo en un archivo de texto llamado `Parqueadero.txt`.
- Al inicio, el programa carga la información almacenada en el archivo `Parqueadero.txt` en una lista para su manipulación durante la ejecución.
- Se utiliza una lista para almacenar la información del vehículo.
- Se han implementado funciones para cada uno de los requisitos funcionales.
- Se ha implementado la gestión de archivos para la persistencia de datos.

## Funcionalidades

- **Registrar entrada de vehículo**: Permite registrar la entrada de un vehículo al estacionamiento solicitando la placa del vehículo y registrando la hora actual de entrada.
- **Registrar salida de vehículo**: Permite registrar la salida de un vehículo del estacionamiento solicitando la placa del vehículo y registrando la hora actual de salida. Calcula el tiempo que el vehículo ha permanecido en el estacionamiento.
- **Buscar vehículo por placa**: Permite buscar un vehículo en el estacionamiento por su placa. Muestra la información del vehículo si se encuentra en el estacionamiento.
- **Listar vehículos**: Muestra todos los vehículos actualmente en el estacionamiento.
- **Salir**: Guarda los registros en el archivo y cierra el programa.

## Ejecución

Para ejecutar el programa, simplemente compila y ejecuta el archivo Haskell en tu entorno de desarrollo favorito.

