# Adversarial-Search-Haskell
Resumen: Búsqueda de estados en juegos con adversario implementado en Haskell y aplicado a algunos juegos


## Objetivo de este documento
Este documento pretende ser solo un manual de uso para la aplicación asociada. No se cubrirán detalles del código ni del funcionamiento del mismo.

## Aclaraciones
El idioma en el que están los textos del programa es inglés, esto es debido a que el paquete gráfico seleccionado para desarrollarlo no soporta acentos ni la existencia de la 'ñ'.

## Partes del programa
La aplicación en general es muy sencillita. Consta de tres tipos de pantallas que cubren todas sus necesidades:

	1. Un menú inicial en el que poder escoger cualquiera de los juegos desarrollados.
	2. Un menú de opciones que diferirá dependiendo del juego seleccionado.
	3. La pantalla de juego del título escogido, listo para jugar.

### Menú inicial
Esta pantalla es simplemente la lista de los títulos seleccionables, tal como muestra la siguiente imagen:
![Menú inicial](img/main-menu "Menú inicial")

Lo único que podemos hacer es pulsar en el juego para cargar el menú de opciones del mismo.

### Menú de opciones
