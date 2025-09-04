# Trabajo final individual

**Maestría Estadística Aplicada: Manejo y Visualización de datos - FCEyE - UNR**

*Estudiante: Pedro Tealdi*

## Historial de partidos de las selecciones argentinas de handball

Se presenta un breve análisis del historial de partidos de las selecciones mayores femenina y masculina de handball de Argentina. Los datos se encuentran de manera abierta en la página oficial de la Confederación Argentina de Handball.

Para este trabajo práctico el enfoque está puesto en implementar herramientas de visualización más que en el análisis propiamente dicho.
Se propuso implementar una Aplicación de Shiny que brinde una visualización dinámica e interactiva para los usuarios.

Origen datos: **https://docs.google.com/spreadsheets/d/1LActdHNtHUnGfaWI3B6-u94SYoaadJqYMJgGKyrIjwk/edit?gid=373917176#gid=373917176**

## Instrucciones para correr la aplicación

Existen dos carpetas principales:

* datos: posee el archivo de datos con extensión xlsx.
* codigo: se encuentran los archivos con los algoritmos desarrollados para el TP.

El código se descompone en 4 archivos .r.

1. visualizacion_main.r: archivo principal que importa al resto, **correr este archivo para levantar la aplicación.**
2. datos_inputYprocesamiento.r: algoritmo para ingreso, limpieza y preparamiento de datos, a partir de este se importan las estructuras de datos al main.
3. visualizacion_funciones.r: funciones para visualizaciones específicas.
4. visualizacion_textos.r: textos que se visualizan en el panel, se concentran aquí para reducir el archivo principal.
