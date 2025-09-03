
#### Manejo y Visualización de Datos ####
### Maestría en Estadística Aplicada - FCEyE UNR
## TP Individual: Pedro Tealdi

# Origen datos: https://docs.google.com/spreadsheets/d/1LActdHNtHUnGfaWI3B6-u94SYoaadJqYMJgGKyrIjwk/edit?gid=373917176#gid=373917176

# Diseño e implementación de visualizaciones: Shiny #

# Maqueta:
# 1. Titulo y subtitulo generales
# 2. Tres pestañas
#   a. Introducción: Breve descripción del análisis y objetivos, disposición de las visualización, definiciones más importantes (recorte de datos)
#   b. Datos fem:
#       i. Panel izquierdo: 
#           - 3 seleccionadores: Tipo de Torneo (Amistoso, No Amistoso) (Uno o ambos), Tipo de adversario (Selección, Club) (Uno o ambos) y qué visualizar en evolución temporal (Partidos ganados, perdidos, empatados o ambos) (Uno o ambos)
#           - Tabla de datos: Partidos totales, partidos ganados, partidos perdidos
#       ii. Panel central: Gráfico evolución temporal segun filtros
#   c. Datos masc: idem

# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------


# Deshabilitar warnings de sintaxis
# nolint start

library(tidyverse)
library(stringr)
library(shiny)
# library(shinythemes)
library(bslib)
library(plotly)

# Import datos preprocesados
source("codigo/datos_inputYprocesamiento.r")
# Importar textos
source("codigo/visualizacion_textos.r")
# Importar funciones de visualizacion
source("codigo/visualizacion_funciones.r")

# Working directory
# setwd("ruta/al/directorio")

# Datos generales
fecha_inicio_fem <- resumen_ambasRamas[resumen_ambasRamas$rama == "Femenino", "min_fecha"]
fecha_inicio_masc <- resumen_ambasRamas[resumen_ambasRamas$rama == "Masculino", "min_fecha"]
path_logoCAH <- "logo_cah_2.png"

# Interfaz de usuario
ui <- fluidPage(
    # theme = shinytheme("flatly"),
    theme = bs_theme(
        version = 4,
        bootswatch = "journal", # Opciones: cerulean, .cosmo, !cyborg, !darkly, .flatly, journal, litera, lumen, paper, readable, sandstone, simplex, slate, !spacelab, !superhero, united
        primary = "#98aceeff",
        base_font = font_google("Inter")
    ),
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
    # Diseño tabs: color texto en pestañas
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
    tabsetPanel(id = "mainTabs",
        tabPanel("Introducción"
                , tags$div(intro_texto, # Texto incial
                          style = "text-align: justify; text-justify: inter-word; font-size: 14px; margin-top: 20px; width: 80%; margin-left: auto; margin-right: auto;")
                , tags$div("Resumen general", # Tabla resumen
                           style = "text-align: center; font-size: 16px; font-weight: bold; margin-top: 20px; width: 80%; margin-left: auto; margin-right: auto;")
                , tags$div(
                    tableOutput("intro_resumenTabla"),
                    style = "text-align: center; font-size: 16px; margin-top: 20px; width: 80%; margin-left: auto; margin-right: auto;"
                ),
                # Logo CAH  abajo a la izquierda
                tags$div(
                    style = "position: fixed; bottom: 8px; right: 8px; z-index: 1000;",
                    tags$img(src = path_logoCAH, alt = "Logo CAH", style = "height:60px; max-width:120px;")
                )
        ),

        tabPanel("Selección femenina",
            tags$div(paste0("Partidos de la selección mayor femenina desde: ", as.character(fecha_inicio_fem)),
                    style = "text-align: center; font-size: 14px; margin-top: 20px; margin-bottom: 20px; font-weight: bold;"),
            fluidRow( # Division tab: both columns inside the same fluidRow
                column(4, style = "display:flex; justify-content:center;", # Panel izquierdo con filtros
                    wellPanel(
                        style = "width:100%; max-width:320px; display:flex; flex-direction:column; align-items:center;",
                        tags$div("Filtros para la selección femenina.
                            Seleccionar al menos una opción por selector.",
                            style = "text-align: center; font-size: 14px; margin-top: 20px;"),
                        selectInput("fem_tipo_torneo", "Tipo de Torneo",
                            choices = c("Amistoso" = TRUE, "No Amistoso" = FALSE),
                            selected = c(TRUE, FALSE),
                            multiple = TRUE, width = "90%"),
                        selectInput("fem_tipo_rival", "Tipo de adversario",
                            choices = c("Selección", "Club"),
                            selected = c("Selección", "Club"),
                            multiple = TRUE, width = "90%"),
                        checkboxGroupInput("fem_que_visualizar", "Qué visualizar",
                                            choices = c("Partidos ganados" = "ganados",
                                                        "Partidos perdidos" = "perdidos",
                                                        "Partidos empatados" = "empatados",
                                                        "Todos" = "todos"),
                                            selected = "todos", width = "90%"),
                        # Comportamiento: solo una opción activa a la vez. Si se intenta dejar sin opciones, se vuelve a marcar la última.
                        tags$script(HTML("
                            $(document).on('change', 'input[name=\"fem_que_visualizar\"]', function() {
                            var $this = $(this);
                            // Si se marca, desmarcar todas las demás (comportamiento tipo radio)
                            if ($this.prop('checked')) {
                                $('input[name=\"fem_que_visualizar\"]').not($this).prop('checked', false);
                                Shiny.setInputValue('fem_que_visualizar', [$this.val()], {priority: 'event'});
                            } else {
                                // Si quedó ninguna marcada, volver a marcar la que el usuario intentó desmarcar
                                var checked = $('input[name=\"fem_que_visualizar\"]:checked');
                                if (checked.length === 0) {
                                $this.prop('checked', true);
                                Shiny.setInputValue('fem_que_visualizar', [$this.val()], {priority: 'event'});
                                } else {
                                // Actualizar Shiny con la selección actual (por seguridad)
                                Shiny.setInputValue('fem_que_visualizar', checked.map(function(){ return this.value; }).get(), {priority: 'event'});
                                }
                            }
                            });
                        ")),
                        hr(),
                        tags$div("Resumen de partidos", style = "font-weight: bold; text-align: center; margin-bottom: 8px;"),
                        # CSS local para poner en negrita la última fila de la tabla
                        tags$style(HTML("#fem_resumenTabla table tr:last-child td { font-weight: bold; }")),
                        div(style = "width:100%; display:flex; justify-content:center;",
                            tableOutput("fem_resumenTabla")
                        )
                    )
                ),
                column(8,
                        # Panel central: barra continua para seleccionar el tiempo y gráfico de evolución
                    div(style = "display:flex; flex-direction:column; justify-content:center; align-items:center; height:100%;",
                        # Slider de fechas (rango continuo)
                        sliderInput("fem_rango_fecha", "Rango de fechas:",
                            min = min(as.Date(datos_fem$fecha), na.rm = TRUE),
                            max = max(as.Date(datos_fem$fecha), na.rm = TRUE),
                            value = c(min(as.Date(datos_fem$fecha), na.rm = TRUE), max(as.Date(datos_fem$fecha), na.rm = TRUE)),
                            timeFormat = "%Y-%m-%d",
                            width = "90%"),
                        div(style = "width:100%; max-width:900px; margin-top:8px;",
                            plotly::plotlyOutput("fem_evolucion", height = "500px")
                        )
                    )
                )
            )
        ),

        tabPanel("Selección masculina",
            tags$div(paste0("Partidos de la selección mayor masculina desde: ", as.character(fecha_inicio_masc)),
                     style = "text-align: center; font-size: 14px; margin-top: 20px; margin-bottom: 20px; font-weight: bold;"),
            fluidRow(
                column(4, style = "display:flex; justify-content:center;",
                    wellPanel(
                        style = "width:100%; max-width:320px; display:flex; flex-direction:column; align-items:center;",
                        tags$div("Filtros para la selección masculina.
                            Seleccionar al menos una opción por selector.",
                            style = "text-align: center; font-size: 14px; margin-top: 20px;"),
                        selectInput("masc_tipo_torneo", "Tipo de Torneo",
                            choices = c("Amistoso" = TRUE, "No Amistoso" = FALSE),
                            selected = c(TRUE, FALSE),
                            multiple = TRUE, width = "90%"),
                        selectInput("masc_tipo_rival", "Tipo de adversario",
                            choices = c("Selección", "Club"),
                            selected = c("Selección", "Club"),
                            multiple = TRUE, width = "90%"),
                        checkboxGroupInput("masc_que_visualizar", "Qué visualizar",
                                            choices = c("Partidos ganados" = "ganados",
                                                        "Partidos perdidos" = "perdidos",
                                                        "Partidos empatados" = "empatados",
                                                        "Todos" = "todos"),
                                            selected = "todos", width = "90%"),
                        # Comportamiento: solo una opción activa a la vez. Si se intenta dejar sin opciones, se vuelve a marcar la última.
                        tags$script(HTML("
                            $(document).on('change', 'input[name=\"masc_que_visualizar\"]', function() {
                            var $this = $(this);
                            // Si se marca, desmarcar todas las demás (comportamiento tipo radio)
                            if ($this.prop('checked')) {
                                $('input[name=\"masc_que_visualizar\"]').not($this).prop('checked', false);
                                Shiny.setInputValue('masc_que_visualizar', [$this.val()], {priority: 'event'});
                            } else {
                                // Si quedó ninguna marcada, volver a marcar la que el usuario intentó desmarcar
                                var checked = $('input[name=\"masc_que_visualizar\"]:checked');
                                if (checked.length === 0) {
                                $this.prop('checked', true);
                                Shiny.setInputValue('masc_que_visualizar', [$this.val()], {priority: 'event'});
                                } else {
                                // Actualizar Shiny con la selección actual (por seguridad)
                                Shiny.setInputValue('masc_que_visualizar', checked.map(function(){ return this.value; }).get(), {priority: 'event'});
                                }
                            }
                            });
                        ")),
                        hr(),
                        tags$div("Resumen de partidos", style = "font-weight: bold; text-align: center; margin-bottom: 8px;"),
                        # CSS local para poner en negrita la última fila de la tabla
                        tags$style(HTML("#masc_resumenTabla table tr:last-child td { font-weight: bold; }")),
                        div(style = "width:100%; display:flex; justify-content:center;",
                            tableOutput("masc_resumenTabla")
                        )
                    )
                ),
                column(8,
                    # Panel central: barra continua para seleccionar el tiempo y gráfico de evolución
                    div(style = "display:flex; flex-direction:column; justify-content:center; align-items:center; height:100%;",
                        # Slider de fechas (rango continuo)
                        sliderInput("masc_rango_fecha", "Rango de fechas:",
                            min = min(as.Date(datos_masc$fecha), na.rm = TRUE),
                            max = max(as.Date(datos_masc$fecha), na.rm = TRUE),
                            value = c(min(as.Date(datos_masc$fecha), na.rm = TRUE), max(as.Date(datos_masc$fecha), na.rm = TRUE)),
                            timeFormat = "%Y-%m-%d",
                            width = "90%"),
                        div(style = "width:100%; max-width:900px; margin-top:8px;",
                            plotly::plotlyOutput("masc_evolucion", height = "500px")
                        )
                    )
                )
            )
        )
    )
)



# Servidor
server <- function(input, output) {
    # Panel intro
    output$intro_resumenTabla <- renderTable({
        # Resumen de los datos
        resumen_datos <- resumen_ambasRamas[, c("rama", "cant_partidos", "cant_partidos_ganados", "cant_partidos_perdidos", "mean_goles_arg", "mean_goles_adv", "min_fecha")]
        names(resumen_datos) <- c("Rama", "Cantidad de partidos", "Cantidad de partidos ganados", "Cantidad de partidos perdidos", "Media goles (Argentina)", "Media goles (Rival)", "Primera fecha registrada")
        resumen_datos
    }, striped = FALSE, hover = TRUE, bordered = TRUE, align = "c")
        # Centrar la tabla entera con CSS en <head>
        insertUI(
            selector = "head",
            ui = tags$style(HTML("#intro_resumenTabla table { margin-left: auto; margin-right: auto; }")),
            immediate = TRUE
        )
    
    # Panel fem
    ## Filtrar datos según inputs
    datos_filtrados_fem <- reactive({
        req(input$fem_tipo_torneo, input$fem_tipo_rival, input$fem_rango_fecha)
        fecha_min <- as.Date(input$fem_rango_fecha[1])
        fecha_max <- as.Date(input$fem_rango_fecha[2])
        datos_fem %>%
            filter(amistoso %in% input$fem_tipo_torneo,
                    tipo_adv %in% input$fem_tipo_rival,
                    as.Date(fecha) >= fecha_min,
                    as.Date(fecha) <= fecha_max)
    })
    ## Tabla resumen
    output$fem_resumenTabla <- renderTable({
        df <- datos_filtrados_fem()
        # Asegurar que la tabla responda aunque no haya filas
        total_partidos <- nrow(df)
        partidos_ganados <- sum(df$goles_arg > df$goles_adv, na.rm = TRUE)
        partidos_perdidos <- sum(df$goles_arg < df$goles_adv, na.rm = TRUE)
        partidos_empatados <- sum(df$goles_arg == df$goles_adv, na.rm = TRUE)

        resumen <- data.frame(
            Métrica = c("Partidos ganados", "Partidos perdidos", "Partidos empatados", "Partidos totales"),
            Valor = c(partidos_ganados, partidos_perdidos, partidos_empatados, total_partidos),
            stringsAsFactors = FALSE
        )
        resumen
    }, striped = FALSE, hover = TRUE, bordered = TRUE, align = "c")

    ## Centrar la tabla entera con CSS en <head>
    insertUI(
        selector = "head",
        ui = tags$style(HTML("#fem_resumenTabla table { margin-left: auto; margin-right: auto; }")),
        immediate = TRUE
    )

    ## Plot evolucion interactivo (hover: adversario, lugar, torneo)
    output$fem_evolucion <- plotly::renderPlotly({
        p <- crear_grafico_evolucion_por_anio(datos_filtrados_fem(), input$fem_que_visualizar)
        plotly::ggplotly(p, tooltip = c("adversario", "lugar", "torneo", "text")) %>%
            plotly::layout(hovermode = "closest")
    })

    # Panel masc
    ## Filtrar datos según inputs
    datos_filtrados_masc <- reactive({
        req(input$masc_tipo_torneo, input$masc_tipo_rival, input$masc_rango_fecha)
        fecha_min <- as.Date(input$masc_rango_fecha[1])
        fecha_max <- as.Date(input$masc_rango_fecha[2])
        datos_masc %>%
            filter(amistoso %in% input$masc_tipo_torneo,
                    tipo_adv %in% input$masc_tipo_rival,
                    as.Date(fecha) >= fecha_min,
                    as.Date(fecha) <= fecha_max)
    })

    ## Tabla resumen
    output$masc_resumenTabla <- renderTable({
        df <- datos_filtrados_masc()
        total_partidos <- nrow(df)
        partidos_ganados <- sum(df$goles_arg > df$goles_adv)
        partidos_perdidos <- sum(df$goles_arg < df$goles_adv)
        partidos_empatados <- sum(df$goles_arg == df$goles_adv)

        resumen <- data.frame(
            Métrica = c("Partidos ganados", "Partidos perdidos", "Partidos empatados", "Partidos totales"),
            Valor = c(partidos_ganados, partidos_perdidos, partidos_empatados, total_partidos)
        )
        resumen
    }, striped = FALSE, hover = TRUE, bordered = TRUE, align = "c")

    ## Centrar la tabla entera con CSS en <head>
    insertUI(
        selector = "head",
        ui = tags$style(HTML("#masc_resumenTabla table { margin-left: auto; margin-right: auto; }")),
        immediate = TRUE
    )

    ## Plot evolucion interactivo (hover: adversario, lugar, torneo)
    output$masc_evolucion <- plotly::renderPlotly({
        p <- crear_grafico_evolucion_por_anio(datos_filtrados_masc(), input$masc_que_visualizar)
        plotly::ggplotly(p, tooltip = c("adversario", "lugar", "torneo", "text")) %>%
            plotly::layout(hovermode = "closest")
    })
}


# App Shiny
shinyApp(ui = ui, server = server)
