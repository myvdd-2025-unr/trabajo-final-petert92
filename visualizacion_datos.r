
#### Manejo y Visualización de Datos ####
### Maestría en Estadística Aplicada - FCEyE UNR
## TP Individual: Pedro Tealdi

# Origen datos: https://docs.google.com/spreadsheets/d/1LActdHNtHUnGfaWI3B6-u94SYoaadJqYMJgGKyrIjwk/edit?gid=373917176#gid=373917176

# Diseño e implementación de visualizaciones: Shiny #

# Maqueta:
# 1. Titulo y subtitulo generales
# 2. Cuatro pestañas
#   a. Introducción: Breve descripción del análisis y objetivos, disposición de las visualización, definiciones más importantes (recorte de datos)
#   b. Datos fem
#   c. Datos masc
#   d. Limpieza de datos

# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------


# Deshabilitar warnings de sintaxis
# nolint start

# Import datos_inputYprocesamiendo.r
source("datos_inputYprocesamiento.r")
# Importar textos de visualizacion_textos.r
source("visualizacion_textos.r")

library(tidyverse)
library(readxl)
library(stringr)
library(shiny)

# Working directory
# setwd("ruta/al/directorio")

# Crear la interfaz de usuario
ui <- fluidPage(
    # Titulo principal (constante)
    tags$div("Análisis de Datos de Selecciones Nacionales de Handball", style = "text-align: center; font-size: 21px;font-weight: bold;"),
    tags$div("Historial de partidos de las selecciones mayores femenina y masculina de handball argentinas. - TP Manejo y visualización de datos",
        style = "text-align: center; font-size: 18px;"),
    tags$hr(style = "border-top: 1px solid #ccc; width: 100%; margin: 8px 0;"),
    # Tabs
    tags$head( # Diseño tabs: distribucion
        tags$style(HTML("
            /* Distribuir las pestañas a lo ancho */
            .nav.nav-tabs { display: flex; width: 100%; }
            .nav.nav-tabs > li { flex: 1; text-align: center; }
            .nav.nav-tabs > li > a { display: block; }
            /* Asegurar que el contenedor ocupe ancho completo */
            .tab-content { width: 100%; }
        "))
    ),
    # Diseño tabs: color texto
    tags$head(tags$style(HTML("
        /* Invert tab title colors: active = blue, inactive = black */
        .nav.nav-tabs > li > a { color: black !important; }
        .nav.nav-tabs > li.active > a,
        .nav-tabs .nav-link.active,
        .nav-tabs .nav-item.show .nav-link {
            color: #337ab7 !important; /* Bootstrap primary blue */
        }
    "))),
    # Contenido tabs
    tabsetPanel(  id = "mainTabs",
        tabPanel("Introducción"
                , tags$div(intro_texto, # Texto incial
                          style = "text-align: justify; text-justify: inter-word; font-size: 14px; margin-top: 20px; width: 80%; margin-left: auto; margin-right: auto;")
                , tags$div("Resumen general", # Tabla resumen
                           style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 20px; width: 80%; margin-left: auto; margin-right: auto;")
                , tags$div(
                    tableOutput("intro_resumenTabla"),
                    style = "text-align: center; font-size: 16px; margin-top: 20px; width: 80%; margin-left: auto; margin-right: auto;"
                )
        ),
        tabPanel("Selección femenina",
                        tags$div("Visualización de datos femeninos",
                                            style = "text-align: center; font-size: 13px;")
        ),
        tabPanel("Selección masculina",
                        tags$div("Visualización de datos masculinos",
                                            style = "text-align: center; font-size: 13px;")
        ),
        tabPanel("Limpieza de datos",
                        tags$div("Descripción del proceso de limpieza de datos",
                                            style = "text-align: center; font-size: 13px;")
        )
    )
)

# Definir la lógica del servidor
server <- function(input, output) {
    # Panel intro
    output$intro_resumenTabla <- renderTable({
        # Resumen de los datos
        resumen_datos <- resumen_ambasRamas[, c("rama", "cant_partidos", "mean_goles_arg", "mean_goles_adv", "min_fecha")]
        names(resumen_datos) <- c("Rama", "Cantidad de partidos", "Media goles (Argentina)", "Media goles (Rival)", "Primera fecha registrada")
        resumen_datos
    }, striped = FALSE, hover = TRUE, bordered = TRUE, align = "c")
        # Centrar la tabla entera con CSS en <head>
        insertUI(
            selector = "head",
            ui = tags$style(HTML("#intro_resumenTabla table { margin-left: auto; margin-right: auto; }")),
            immediate = TRUE
        )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
