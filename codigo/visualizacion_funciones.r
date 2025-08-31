
#### Manejo y Visualización de Datos ####
### Maestría en Estadística Aplicada - FCEyE UNR
## TP Individual: Pedro Tealdi

# Origen datos: https://docs.google.com/spreadsheets/d/1LActdHNtHUnGfaWI3B6-u94SYoaadJqYMJgGKyrIjwk/edit?gid=373917176#gid=373917176

# Funciones para visualizaciones #


# imports
library(tidyverse)
library(stringr)

# Función para crear gráficos de evolución temporal (muestra solo algunos valores de x)
crear_grafico_evolucion_por_anio <- function(datos, opcion = c("ganados", "empatados", "perdidos", "todos"), n_ticks = 6) {
    opcion <- as.character(opcion)
    if (length(opcion) > 1) opcion <- opcion[1]
    opcion <- match.arg(opcion, choices = c("ganados", "empatados", "perdidos", "todos"))
    # Fecha -> Date y año
    if (!inherits(datos$fecha, "Date")) datos <- datos %>% mutate(fecha = as.Date(fecha))
    datos <- datos %>% mutate(anio = as.integer(format(fecha, "%Y")))

    # Determinar resultado comparando goles_arg vs goles_adv si están disponibles
    datos <- datos %>% mutate(.resultado = case_when(
        goles_arg > goles_adv ~ "ganados",
        goles_arg == goles_adv ~ "empatados",
        goles_arg < goles_adv ~ "perdidos",
        TRUE ~ NA_character_
    ))

    datos <- datos %>% filter(!is.na(.resultado))
    niveles <- c("ganados", "empatados", "perdidos")
    datos$.resultado <- factor(datos$.resultado, levels = niveles)

    resumen <- datos %>%
        group_by(anio, .resultado) %>%
        summarise(cantidad = n(), .groups = "drop")
    datos[is.na(datos$anio), ] 
    if (opcion != "todos") {
        resumen <- resumen %>% filter(.resultado == opcion)
        titulo <- paste0("Partidos ", opcion, " por año")
        p <- ggplot(resumen, aes(x = anio, y = cantidad)) +
            geom_point(size = 3) +
            geom_line(group = 1) +
            labs(title = titulo, x = "Año", y = "Cantidad de partidos") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
        titulo <- "Partidos por año (por resultado)"
        p <- ggplot(resumen, aes(x = anio, y = cantidad, color = .resultado, shape = .resultado)) +
            geom_point(size = 3, position = position_dodge(width = 0.4)) +
            geom_line(aes(group = .resultado), na.rm = TRUE) +
            scale_color_brewer(palette = "Set1", na.value = "grey") +
            labs(title = titulo, x = "Año", y = "Cantidad de partidos", color = "Resultado", shape = "Resultado") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    p +
    scale_x_continuous(
        breaks = seq(min(resumen$anio, na.rm = TRUE), max(resumen$anio, na.rm = TRUE), by = 5)
    ) +
    scale_y_continuous(
        limits = c(
            0,
            ifelse(is.finite(max(resumen$cantidad, na.rm = TRUE)), ceiling(max(resumen$cantidad, na.rm = TRUE))+1, 1)
        ),
        breaks = function(limits) seq(0, ceiling(limits[2]), by = 2),
        expand = c(0, 0)
    )
}
