library(dplyr)
library(glue)
library(purrr)
library(reactable)
library(readr)
library(tidyr)

# Función para limpiar los datos
limpiar_datos <- function(file_path) {
  # Leer el archivo CSV
  datos <- read_csv(file_path, show_col_types = FALSE)
  
  # Comprobar si las columnas clave están presentes
  columnas_clave <- c("CASILLAS", "NUM_VOTOS_CAN_NREG", "NUM_VOTOS_VALIDOS", 
                      "NUM_VOTOS_NULOS", "TOTAL_VOTOS", "LISTA_NOMINAL")
  faltantes <- setdiff(columnas_clave, names(datos))
  if (length(faltantes) > 0) {
    stop(glue("El archivo {file_path} no contiene las columnas clave: {paste(faltantes, collapse = ', ')}"))
  }
  
  # Identificar las columnas de partidos (entre CASILLAS y NUM_VOTOS_CAN_NREG)
  idx_casillas <- which(names(datos) == "CASILLAS")
  idx_votos_can <- which(names(datos) == "NUM_VOTOS_CAN_NREG")
  columnas_partidos <- names(datos)[(idx_casillas + 1):(idx_votos_can - 1)]
  
  # Verificar si se encontraron partidos
  if (length(columnas_partidos) == 0) {
    stop(glue("No se encontraron columnas de partidos entre CASILLAS y NUM_VOTOS_CAN_NREG en el archivo {file_path}."))
  }
  
  # Informar al usuario
  cat(glue("Procesando {file_path}: {length(columnas_partidos)} partidos detectados ({paste(columnas_partidos, collapse = ', ')})\n"))
  
  # Convertir de ancho a largo solo las columnas de partidos
  datos_limpios <- datos %>%
    pivot_longer(
      cols = all_of(columnas_partidos), # Solo las columnas identificadas como partidos
      names_to = "Partido",            # Nombre de la nueva columna para los partidos
      values_to = "Votos"              # Nombre de la nueva columna para los votos
    ) %>%
    # Mantener columnas clave y reorganizarlas
    select(ID_ESTADO, NOMBRE_ESTADO, ID_MUNICIPIO, MUNICIPIO, Partido, Votos, 
           NUM_VOTOS_VALIDOS, NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL)
  
  return(datos_limpios)
}

limpiar_datos_2018 <- function(file_path) {
  # Leer el archivo CSV
  datos <- read_csv(file_path, show_col_types = FALSE)
  
  # Convertir ID_MUNICIPIO a character para evitar conflictos
  if ("ID_MUNICIPIO" %in% names(datos)) {
    datos <- datos %>%
      mutate(ID_MUNICIPIO = as.numeric(ID_MUNICIPIO))
  }
  
  # Comprobar si las columnas clave están presentes
  columnas_clave <- c("CASILLAS", "NUM_VOTOS_CAN_NREG", "NUM_VOTOS_VALIDOS", 
                      "NUM_VOTOS_NULOS", "TOTAL_VOTOS", "LISTA_NOMINAL")
  faltantes <- setdiff(columnas_clave, names(datos))
  if (length(faltantes) > 0) {
    stop(glue("El archivo {file_path} no contiene las columnas clave: {paste(faltantes, collapse = ', ')}"))
  }
  
  # Identificar las columnas de partidos (entre CASILLAS y NUM_VOTOS_CAN_NREG)
  idx_casillas <- which(names(datos) == "CASILLAS")
  idx_votos_can <- which(names(datos) == "NUM_VOTOS_VALIDOS")
  columnas_partidos <- names(datos)[(idx_casillas + 1):(idx_votos_can - 1)]
  
  # Verificar si se encontraron partidos
  if (length(columnas_partidos) == 0) {
    stop(glue("No se encontraron columnas de partidos entre CASILLAS y NUM_VOTOS_CAN_NREG en el archivo {file_path}."))
  }
  
  # Informar al usuario
  cat(glue("Procesando {file_path}: {length(columnas_partidos)} partidos detectados ({paste(columnas_partidos, collapse = ', ')})\n"))
  
  # Convertir de ancho a largo solo las columnas de partidos
  datos_limpios <- datos %>%
    pivot_longer(
      cols = all_of(columnas_partidos), # Solo las columnas identificadas como partidos
      names_to = "Partido",            # Nombre de la nueva columna para los partidos
      values_to = "Votos"              # Nombre de la nueva columna para los votos
    ) %>%
    # Mantener columnas clave y reorganizarlas
    select(ID_ESTADO, NOMBRE_ESTADO, ID_MUNICIPIO, MUNICIPIO, Partido, Votos, 
           NUM_VOTOS_VALIDOS, NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL)
  
  return(datos_limpios)
}



# Función para detectar coaliciones y agregar la columna Coalición
detectar_coaliciones <- function(data) {
  # Crear la columna Coalición
  data <- data %>%
    dplyr::mutate(
      Coalición = ifelse(grepl("_", Partido), "Sí", "No") # Detecta "_" en la columna Partido
    )
  return(data)
}

# Función para añadir una columna de año al principio del tibble
agregar_anio <- function(data, anio) {
  # Agregar la columna Año y reorganizar
  data <- data %>%
    dplyr::mutate(Año = anio) %>%  # Añade la columna Año
    dplyr::relocate(Año, .before = dplyr::everything()) # Mueve la columna Año al inicio
  return(data)
}

procesar_variable <- function(data, columna) {
  # Realizar las sustituciones
  data <- data %>%
    dplyr::mutate(
      !!sym(columna) := dplyr::case_when(
        grepl("^NVA_ALIANZA$", .data[[columna]]) ~ "NUAL", # Sustituir 'NVA_ALIANZA' por 'NUAL'
        grepl("^CAND_IND\\d*$", .data[[columna]]) ~ "Candidatura Independiente", # Sustituir 'CAND_IND' con o sin sufijos
        TRUE ~ .data[[columna]] # Mantener los demás valores sin cambios
      )
    )
  return(data)
}

procesar_variable_can <- function(data, columna) {
  # Realizar las sustituciones
  data <- data %>%
    dplyr::mutate(
      !!sym(columna) := dplyr::case_when(
        grepl("^NVA_ALIANZA$", .data[[columna]]) ~ "NUAL", # Sustituir 'NVA_ALIANZA' por 'NUAL'
        grepl("^CAN_IND\\d*$", .data[[columna]]) ~ "Candidatura Independiente", # Sustituir 'CAND_IND' con o sin sufijos
        TRUE ~ .data[[columna]] # Mantener los demás valores sin cambios
      )
    )
  return(data)
}

# Función para eliminar los NA y 0:
eliminar_na_y_cero <- function(data, condicion = "any") {
  # Verifica que la condición sea válida
  if (!condicion %in% c("any", "all")) {
    stop("La condición debe ser 'any' (al menos una) o 'all' (todas).")
  }
  
  # Filtrar según la condición
  data <- data %>%
    dplyr::filter(
      if (condicion == "any") {
        !apply(. == 0 | is.na(.), 1, any) # Elimina filas donde alguna columna sea NA o 0
      } else {
        !apply(. == 0 | is.na(.), 1, all) # Elimina filas donde todas las columnas sean NA o 0
      }
    )
  return(data)
}

# Función para unir varios tibbles uno debajo del otro
unir_tibbles <- function(...) {
  # Recibir los tibbles como argumentos
  tibbles <- list(...)
  
  # Verificar si todos son tibbles
  if (!all(sapply(tibbles, tibble::is_tibble))) {
    stop("Todos los argumentos deben ser tibbles.")
  }
  
  # Unir los tibbles
  tibble_unido <- dplyr::bind_rows(tibbles)
  
  return(tibble_unido)
}