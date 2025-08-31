
#### Manejo y Visualización de Datos ####
### Maestría en Estadística Aplicada - FCEyE UNR
## TP Individual: Pedro Tealdi

# Origen datos: https://docs.google.com/spreadsheets/d/1LActdHNtHUnGfaWI3B6-u94SYoaadJqYMJgGKyrIjwk/edit?gid=373917176#gid=373917176

# Procesamiento de datos y generacion de estructuras deseadas #

# ------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------

# Deshabilitar warnings de sintaxis
# nolint start

library(tidyverse)
library(readxl)
library(stringr)

# Working directory
# setwd("ruta/al/directorio")


################## IMPORTACION Y LIMPIEZA ##################


# IMPORTAR DATOS: FEMENINO (xlsx)
## Hoja 1 = "FEM PARTIDOS", Desde fila 5 a fila 459, todo como char
datos_fem <- read_excel("HISTORIALES SELECCIONES ADULTAS.xlsx",
                        sheet = "FEM PARTIDOS", skip = 4, col_types = "text", n_max = 455)

# LIMPIEZA
## Quito columnas: "...<numero>"
datos_fem <- datos_fem %>% select(-c(starts_with("...")))
## Renombrar columnas
colnames(datos_fem) <- c("id", "fecha", "lugar", "goles_arg", "goles_adv", "adversario", "observaciones", "torneo")
## Columnas goles a int: primero reemplazo G por 1 y P por -1 y luego filtro por los que no tengan goles
datos_fem <- datos_fem %>%
    mutate(
        goles_arg = case_when(
            str_detect(goles_arg, "G") ~ "1",
            str_detect(goles_arg, "P") ~ "-1",
            TRUE ~ goles_arg
        ),
        goles_adv = case_when(
            str_detect(goles_adv, "G") ~ "1",
            str_detect(goles_adv, "P") ~ "-1",
            TRUE ~ goles_adv
        )
    )
datos_fem <- datos_fem %>%
  mutate(across(c(goles_arg, goles_adv), as.integer))
datos_fem <- datos_fem %>%
  filter(!is.na(goles_arg) | !is.na(goles_adv))
## Cambiar formato fecha dd/mm/aaaa a aaaa-mm-dd cuando corresponda
### Si 'fecha' vino como serial/float convertir a Date
datos_fem <- datos_fem %>%
  mutate(
    fecha = {
      n <- suppressWarnings(as.numeric(fecha))
      ifelse(!is.na(n),
             as.character(as.Date(n, origin = "1899-12-30")),
             fecha)
    }
  )
### Las fechas con "/" tienen formato mm/dd/aaaa: pasar a aaaa-mm-dd. Las demas tienen formato yyyy-dd-mm: pasar a aaaa-mm-dd
datos_fem <- datos_fem |>
  mutate(
    fecha_aux = ifelse(str_detect(as.character(fecha), pattern = "/"),
                       format(as.Date(as.character(fecha), format = "%m/%d/%Y"), "%Y-%m-%d"),
                    format(as.Date(as.character(fecha), format = "%Y-%d-%m"), "%Y-%m-%d")
                    )
    )
# datos_fem[,c("fecha_aux", "fecha")] |> head(10)
datos_fem <- datos_fem |>
  mutate(fecha = fecha_aux) |>
  select(-fecha_aux)

# ACOMODAR DATOS
# print(datos_fem[, c("adversario","tipo_adv")], n= 50)

## Unifico amistoso
# str_subset(datos_fem$torneo, "amistoso|Amistoso|AMISTOSO|test|TEST|Test|exhibición|Exhibición|EXHIBICIÓN")
datos_fem <- datos_fem |>
  mutate(
    torneo = ifelse(str_detect(torneo, "amistoso|Amistoso|AMISTOSO|test|TEST|Test|exhibición|Exhibición|EXHIBICIÓN"),
                    "Amistoso",
                    torneo)
  )
## Creo columna para tipo de adversario
datos_fem <- datos_fem |>
  mutate(
    tipo_adv = ifelse(str_detect(adversario, "\\(.*\\)"),
                      "Club",
                      "Selección")
  )



# IMPORTAR DATOS MASCULINOS (xlsx)
datos_masc <- read_excel("HISTORIALES SELECCIONES ADULTAS.xlsx",
                          sheet = "MASC PARTIDOS", skip = 5, col_types = "text", n_max = 608)
# tail(datos_masc)
## Nombre de columnas
colnames(datos_masc) <- c("id", "fecha", "lugar", "goles_arg", "goles_adv", "adversario", "tipo_partido", "observaciones", "torneo")
## Filas de encabezados: id no numérico
filter(datos_masc, !str_detect(id, "^[0-9.]+$")) |> select(id) # Me quedo con las filas a partir del segundo #
filas_filtrar <- which(datos_masc$id == "#")
datos_masc <- datos_masc[-(1:filas_filtrar[2]), ]
## Cambiar formato fecha dd/mm/aaaa a aaaa-mm-dd cuando corresponda
### Si 'fecha' vino como serial/float convertir a Date
datos_masc <- datos_masc %>%
  mutate(
    fecha = {
      n <- suppressWarnings(as.numeric(fecha))
      ifelse(!is.na(n),
             as.character(as.Date(n, origin = "1899-12-30")),
             fecha)
    }
  )
### Las fechas con "/" tienen formato mm/dd/aaaa: pasar a aaaa-mm-dd. Las demas tienen formato yyyy-dd-mm: pasar a aaaa-mm-dd
datos_masc <- datos_masc |>
  mutate(
    fecha_aux = ifelse(str_detect(as.character(fecha), pattern = "/"),
                       format(as.Date(as.character(fecha), format = "%m/%d/%Y"), "%Y-%m-%d"),
                    format(as.Date(as.character(fecha), format = "%Y-%d-%m"), "%Y-%m-%d")
                    )
    )
# datos_masc[,c("fecha_aux", "fecha")] |> head(10)
datos_masc <- datos_masc |>
  mutate(fecha = fecha_aux) |>
  select(-fecha_aux)
## Columnas goles a int: primero reemplazo G por 1 y P por -1 y luego filtro por los que no tengan goles
datos_masc <- datos_masc %>%
    mutate(
        goles_arg = case_when(
            str_detect(goles_arg, "G") ~ "1",
            str_detect(goles_arg, "P") ~ "-1",
            TRUE ~ goles_arg
        ),
        goles_adv = case_when(
            str_detect(goles_adv, "G") ~ "1",
            str_detect(goles_adv, "P") ~ "-1",
            TRUE ~ goles_adv
        )
    )
datos_masc <- datos_masc %>%
  mutate(across(c(goles_arg, goles_adv), as.integer))
datos_masc <- datos_masc %>%
  filter(!is.na(goles_arg) | !is.na(goles_adv))
# datos_fem |> filter(is.na(goles_arg) | is.na(goles_adv)) |> head(10)
datos_masc <- datos_masc %>%
  filter(!is.na(goles_arg) | !is.na(goles_adv))

# ACOMODAR DATOS
# print(datos_masc[, c("adversario","tipo_partido")], n= 50)

## Unifico amistoso
# str_subset(datos_masc$torneo, "amistoso|Amistoso|AMISTOSO|test|TEST|Test|exhibición|Exhibición|EXHIBICIÓN")
datos_masc <- datos_masc |>
  mutate(
    torneo = ifelse(str_detect(torneo, "amistoso|Amistoso|AMISTOSO|test|TEST|Test|exhibición|Exhibición|EXHIBICIÓN"),
                    "Amistoso",
                    torneo)
  )
## Creo columna para tipo de adversario
datos_masc <- datos_masc |>
  mutate(
    tipo_adv = ifelse(str_detect(tipo_partido, "SN"),
                      "Selección",
                      "Club")
  )




################## GENERACION DE DATASETS DESEADOS ##################

# Tabla resulmen ambas ramas
## Cant partidos, cant partidos amistosos, cant partidos no amistosos, cant partidos ganados, cant partidos perdidos
## , mean goles a favor y en contra, mean goles a favor y en contra partidos amistosos y no amistoso, mean goles a favor y en contra partidos seleccion y no seleccion
## , min fecha
resumen_fem <- datos_fem %>%
  summarise(
    cant_partidos = n(),
    cant_partidos_amistosos = sum(torneo == "Amistoso"),
    cant_partidos_no_amistosos = sum(torneo != "Amistoso"),
    cant_partidos_seleccion = sum(tipo_adv == "Selección"),
    cant_partidos_club = sum(tipo_adv == "Club"),
    cant_partidos_ganados = sum(goles_arg > goles_adv, na.rm = TRUE),
    cant_partidos_perdidos = sum(goles_arg < goles_adv, na.rm = TRUE),
    cant_partidos_empatados = sum(goles_arg == goles_adv, na.rm = TRUE),
    min_fecha = min(fecha, na.rm = TRUE),
    max_goles_arg = max(goles_arg, na.rm = TRUE),
    max_goles_adv = max(goles_adv, na.rm = TRUE),
    min_goles_arg = min(goles_arg, na.rm = TRUE),
    min_goles_adv = min(goles_adv, na.rm = TRUE),
    mean_goles_arg = mean(goles_arg, na.rm = TRUE),
    mean_goles_adv = mean(goles_adv, na.rm = TRUE),
    mean_goles_arg_amistosos = mean(goles_arg[torneo == "Amistoso"], na.rm = TRUE),
    mean_goles_adv_amistosos = mean(goles_adv[torneo == "Amistoso"], na.rm = TRUE),
    mean_goles_arg_no_amistosos = mean(goles_arg[torneo != "Amistoso"], na.rm = TRUE),
    mean_goles_adv_no_amistosos = mean(goles_adv[torneo != "Amistoso"], na.rm = TRUE),
    mean_goles_arg_seleccion = mean(goles_arg[tipo_adv == "Selección"], na.rm = TRUE),
    mean_goles_adv_seleccion = mean(goles_adv[tipo_adv == "Selección"], na.rm = TRUE),
    mean_goles_arg_no_seleccion = mean(goles_arg[tipo_adv != "Selección"], na.rm = TRUE),
    mean_goles_adv_no_seleccion = mean(goles_adv[tipo_adv != "Selección"], na.rm = TRUE)
  )

resumen_masc <- datos_masc %>%
  summarise(
    cant_partidos = n(),
    cant_partidos_amistosos = sum(torneo == "Amistoso"),
    cant_partidos_no_amistosos = sum(torneo != "Amistoso"),
    cant_partidos_seleccion = sum(tipo_adv == "Selección"),
    cant_partidos_club = sum(tipo_adv == "Club"),
    cant_partidos_ganados = sum(goles_arg > goles_adv, na.rm = TRUE),
    cant_partidos_perdidos = sum(goles_arg < goles_adv, na.rm = TRUE),
    cant_partidos_empatados = sum(goles_arg == goles_adv, na.rm = TRUE),
    min_fecha = min(fecha, na.rm = TRUE),
    max_goles_arg = max(goles_arg, na.rm = TRUE),
    max_goles_adv = max(goles_adv, na.rm = TRUE),
    min_goles_arg = min(goles_arg, na.rm = TRUE),
    min_goles_adv = min(goles_adv, na.rm = TRUE),
    mean_goles_arg = mean(goles_arg, na.rm = TRUE),
    mean_goles_adv = mean(goles_adv, na.rm = TRUE),
    mean_goles_arg_amistosos = mean(goles_arg[torneo == "Amistoso"], na.rm = TRUE),
    mean_goles_adv_amistosos = mean(goles_adv[torneo == "Amistoso"], na.rm = TRUE),
    mean_goles_arg_no_amistosos = mean(goles_arg[torneo != "Amistoso"], na.rm = TRUE),
    mean_goles_adv_no_amistosos = mean(goles_adv[torneo != "Amistoso"], na.rm = TRUE),
    mean_goles_arg_seleccion = mean(goles_arg[tipo_adv == "Selección"], na.rm = TRUE),
    mean_goles_adv_seleccion = mean(goles_adv[tipo_adv == "Selección"], na.rm = TRUE),
    mean_goles_arg_no_seleccion = mean(goles_arg[tipo_adv != "Selección"], na.rm = TRUE),
    mean_goles_adv_no_seleccion = mean(goles_adv[tipo_adv != "Selección"], na.rm = TRUE)
  )

resumen_ambasRamas <- bind_rows(resumen_fem, resumen_masc, .id = "rama")
resumen_ambasRamas <- resumen_ambasRamas %>%
  mutate(rama = case_when(
    rama == "1" ~ "Femenino",
    rama == "2" ~ "Masculino"
  ))
# str(resumen_ambasRamas)

print("Generacion de:")
print("1. Dataframes de datos: datos_fem, datos_masc")
print("2  Dataframes de resumenes: resumen_fem, resumen_masc, resumen_ambasRamas")
