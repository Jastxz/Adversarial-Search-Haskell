# Adversarial-Search-Haskell
Resumen: Búsqueda de estados en juegos con adversario implementado en Haskell y aplicado a algunos juegos


## Objetivo de este documento
Este documento pretende ser solo un manual de uso para la aplicación asociada. No se cubrirán detalles del código ni del funcionamiento del mismo.

## Aclaraciones sobre el proyecto
Junto con el proyecto, viene un archivo zip que contiene el programa compilado y listo para ejecutarse en Windows. Trae también un archivo dll necesario para su correcto funcionamiento.

El idioma en el que están los textos del programa es inglés, esto es debido a que el paquete gráfico seleccionado para desarrollarlo no soporta *acentos* ni la existencia de la letra *ñ*.

## Partes del programa
La aplicación en general es muy sencillita. Consta de tres tipos de pantallas que cubren todas sus necesidades:

1. Un menú inicial en el que poder escoger cualquiera de los juegos desarrollados.
2. Un menú de opciones que diferirá dependiendo del juego seleccionado.
3. La pantalla de juego del título escogido, listo para jugar.
4. Pantalla de fin del juego.
5. Un menú que te muestra las partidas guardadas disponibles.
6. Pantalla de errores.

### Menú inicial
Esta pantalla es simplemente la lista de los títulos seleccionables, tal como muestra la siguiente imagen:
![Menú inicial](file://img/main-menu "Menú inicial")

Lo único que podemos hacer es pulsar en el juego para cargar el menú de opciones del mismo.

### Menú de opciones
Según el juego escogido pueden aparecer unas opciones u otras, pero en general siempre mantiene la misma estructura. Para abarcar todos los ejemplos posibles hasta la fecha (2/12/2022) se mostrarán los ejemplos con las imágenes del menú de opciones del juego del *Ratón vs Gatos*.

Todas las pantallas de opciones se componen de la misma forma:

- Una serie de botones:
1. Un botón para volver a la selección de juegos.
![Botón del menú inicial](file://img/main-menu-btn "Botón para volver al menú inicial")
2. Un botón para comenzar una partida con las opciones seleccionadas.
![Botón de comenzar](file://img/start-btn "Botón para comenzar la partida")
3. Un botón para cargar una partida guardada de cualquier juego.
![Botón de cargar](file://img/load-btn "Botón para cargar una partida")
- Bloque de opción, que puede ser del tipo:
1. Varias checkbox para determinar la elección de ese ajuste específico.
![Opción checkbox](file://img/checkbox-option "Opción mediante checkboxes")
2. Un tablero que se habilita o deshabilita según alguna de las opciones anteriores y que permite la dispoción inicial de **una** pieza en el tablero. (Implementado solo en el 'Ratón vs Gatos' a fecha 2/12/2022)
![Tablero de opciones](file://img/options-board "Tablero de las opciones")

### Pantalla de juego
Lo único variable cuando se carga el juego elegido es la estructura del tablero mostrada. Todos los juegos tienen la misma cantidad de botones y en todos los juegos lo que podemos hacer como mucho es pulsar en la pantalla.

Para todos los juegos podemos dividir la pantalla de juego en:

- Una serie de botones:
1. Un botón para volver al menú de opciones.
![Botón del menú de opciones](file://img/options-btn "Botón para volver al menú de opciones")
2. Un botón para deshacer **un** movimiento.
![Botón de volver](file://img/back-btn "Botón para deshacer un solo movimiento")
3. Un botón para cargar una partida guardada de cualquier juego.
![Botón de cargar 2](file://img/load-btn-2 "Botón para cargar una partida")
4. Un botón para guardar la partida actual.
![Botón de guardar](file://img/save-btn "Botón para guardar una partida")
- Un par de mensajes:
1. Un mensaje que te indica a quién le toca.
![Indicación 1](file://img/indication "Mensaje de indicación de turno")
2. Un mensaje que te proporciona información del juego.
![Indicación 2](file://img/indication-2 "Mensaje con información de la partida")
- Un tablero sobre el que jugar.
![Tablero](file://img/board "Tablero")

### Pantalla de fin del juego
Esta pantalla la veremos cada vez que se acabe el juego ejecutado. Simplemente aparecerá un mensaje por pantalla que nos especificará quién ha ganado, si tú o la máquina.
![Pantalla de fin del juego](file://img/endgame-screen "Pantalla de fin del juego")

### Menú de cargar partida
Un pequeño menú que te muestra los archivos guardados disponibles y te deja escoger entre ellos. Vale la pena añadir que si intentas cargar una documento que no sea de tipo *.txt* o que no tenga el mismo formato que las partidas guardadas, simplemente fallará.
![Menú de cargar partida](file://img/load-menu "Menú de cargar partida")

### Pantalla de errores
Exactamente igual que la de fin del juego pero con mensajes variados sobre posibles errores que en teoría no deben ocurrir.

## Efectos, efectos colaterales de algunos botones y lugar de almacenamiento de archivos
Hay algunos botones que, por la forma en la que se han decidido implementar o su propia naturaleza, pueden tener efectos no esperados en primera instancia.

- Botón de vuelta al menú inicial. Si habías escogido alguna opción de algún juego, este botón las reseteará lógicamente.
- Botón de vuelta al menú de opciones. Si has empezado una partida y pulsas este botón, todas las opciones se resetearán y cuando entres de nuevo se habrá creado una nueva partida.
- Botón de deshacer un movimiento. Si existe el archivo temporal que este botón usa para volver atrás en el tiempo un paso, al pulsarlo lo cargará. Pongámos un ejemplo: Acabas de echar una partidita al *3 en raya* y ahora vas a jugar un rato a las *damas*; pues si pulsas este botón al comenzar la partida y sin haber movido aún, cargarás el último movimiento del *3 en raya*. Si el archivo no existe, al pulsar el botón volverás al menú de selección de títulos.

También hay algunos archivos que es importante que sepas dónde se crean o se encuentran ubicados. Básicamente el documento temporal del botón de volver y las partidas guardadas.

Respecto del archivo temporal, se creará (o debe crearse) en la raíz del disco en el que tengas ubicado el portable del juego. Pudiéndolo borrar cuando estimes necesario en teoría.
El directorio de partidas guardadas se crea en el mismo lugar donde tengas la aplicación. El programa guardará en esa carpeta todas las partidas que guardes automáticamente. También puedes modificar esos ficheros o borrarlos si gustas.

## Contacto
Si quieres contactar conmigo puedes mandarme un correo a *javicraft14 arroba gmail punto com* o abrir un ticket en este repositorio. Siempre estoy abierto a sugerencias y a arreglar los errores del programa :).