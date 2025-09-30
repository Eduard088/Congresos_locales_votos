library(dplyr)
library(glue)
library(purrr)
library(reactable)
library(readr)
library(tidyr)
library(stringr)

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
  
  # Forzar tipos homogéneos (recomendado: convertir ID_DISTRITO y otras columnas clave a character)
  datos <- datos %>%
    mutate(
      ID_ESTADO = as.character(ID_ESTADO),
      ID_DISTRITO = as.character(ID_DISTRITO),
      NUM_VOTOS_VALIDOS = as.numeric(NUM_VOTOS_VALIDOS),
      NUM_VOTOS_CAN_NREG = as.numeric(NUM_VOTOS_CAN_NREG),
      NUM_VOTOS_NULOS = as.numeric(NUM_VOTOS_NULOS),
      TOTAL_VOTOS = as.numeric(TOTAL_VOTOS),
      LISTA_NOMINAL = as.numeric(LISTA_NOMINAL)
    )
  
  # Convertir de ancho a largo solo las columnas de partidos
  datos_limpios <- datos %>%
    pivot_longer(
      cols = all_of(columnas_partidos),
      names_to = "Partido",
      values_to = "Votos"
    ) %>%
    select(ID_ESTADO, NOMBRE_ESTADO, ID_DISTRITO, CABECERA_DISTRITAL, Partido, Votos, 
           NUM_VOTOS_VALIDOS, NUM_VOTOS_CAN_NREG, NUM_VOTOS_NULOS, TOTAL_VOTOS, LISTA_NOMINAL)
  
  return(datos_limpios)
}

limpiar_datos_2018 <- function(file_path) {
  # Leer el archivo CSV
  datos <- read_csv(file_path, show_col_types = FALSE)
  
  # Convertir ID_MUNICIPIO a character para evitar conflictos
  if ("ID_DISTRITO_LOCAL" %in% names(datos)) {
    datos <- datos %>%
      mutate(ID_DISTRITO_LOCAL = as.numeric(ID_DISTRITO_LOCAL))
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
  
  # Forzar tipos homogéneos (recomendado: convertir ID_DISTRITO y otras columnas clave a character)
  datos <- datos %>%
    mutate(
      ID_ESTADO = as.character(ID_ESTADO),
      ID_DISTRITO_LOCAL = as.character(ID_DISTRITO_LOCAL),
      NUM_VOTOS_VALIDOS = as.numeric(NUM_VOTOS_VALIDOS),
      NUM_VOTOS_CAN_NREG = as.numeric(NUM_VOTOS_CAN_NREG),
      NUM_VOTOS_NULOS = as.numeric(NUM_VOTOS_NULOS),
      TOTAL_VOTOS = as.numeric(TOTAL_VOTOS),
      LISTA_NOMINAL = as.numeric(LISTA_NOMINAL)
    )  
  
  # Convertir de ancho a largo solo las columnas de partidos
  datos_limpios <- datos %>%
    pivot_longer(
      cols = all_of(columnas_partidos), # Solo las columnas identificadas como partidos
      names_to = "Partido",            # Nombre de la nueva columna para los partidos
      values_to = "Votos"              # Nombre de la nueva columna para los votos
    ) %>%
    # Mantener columnas clave y reorganizarlas
    select(ID_ESTADO, NOMBRE_ESTADO, ID_DISTRITO_LOCAL, CABECERA_DISTRITAL_LOCAL, Partido, Votos, 
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



calcular_votos_coalicion <- function(datos_2015) {
  
  # Función auxiliar para determinar si un partido es coalición
  es_coalicion <- function(partido) {
    # No es coalición si:
    # 1. Es "NVA_ALIANZA" (partido individual)
    # 2. Comienza con "CAND_IND" seguido de número
    # 3. No contiene "_"
    
    if (is.na(partido)) return(FALSE)
    
    # Casos especiales que NO son coaliciones
    if (partido == "NVA_ALIANZA") return(FALSE)
    if (str_detect(partido, "^CAND_IND\\d+$")) return(FALSE)
    
    # Es coalición si contiene "_"
    return(str_detect(partido, "_"))
  }
  
  # Función auxiliar para obtener partidos componentes de una coalición
  obtener_partidos_componentes <- function(coalicion) {
    if (is.na(coalicion)) return(character(0))
    return(str_split(coalicion, "_")[[1]])
  }
  
  # Función auxiliar para calcular votos de coalición en un distrito específico
  calcular_votos_distrito <- function(datos_distrito) {
    
    # Crear diccionario de votos por partido en este distrito
    votos_por_partido <- setNames(datos_distrito$Votos, datos_distrito$Partido)
    
    # Calcular votos_coalicion para cada fila
    datos_distrito$votos_coalicion <- sapply(1:nrow(datos_distrito), function(i) {
      partido <- datos_distrito$Partido[i]
      votos_originales <- datos_distrito$Votos[i]
      
      # Si no es coalición, usar votos originales
      if (!es_coalicion(partido)) {
        return(ifelse(is.na(votos_originales), 0, votos_originales))
      }
      
      # Si es coalición, sumar votos de partidos componentes más votos propios
      partidos_componentes <- obtener_partidos_componentes(partido)
      
      # Sumar votos de cada partido componente
      votos_componentes <- sum(sapply(partidos_componentes, function(p) {
        voto <- votos_por_partido[p]
        return(ifelse(is.na(voto), 0, voto))
      }), na.rm = TRUE)
      
      # Sumar votos propios de la coalición (si existen)
      votos_propios <- ifelse(is.na(votos_originales), 0, votos_originales)
      
      return(votos_componentes + votos_propios)
    })
    
    return(datos_distrito)
  }
  
  # Aplicar el cálculo agrupado por ID_ESTADO, ID_DISTRITO y CABECERA_DISTRITAL
  resultado <- datos_2015 %>%
    group_by(ID_ESTADO, NOMBRE_ESTADO, ID_DISTRITO, CABECERA_DISTRITAL) %>%
    group_modify(~ calcular_votos_distrito(.x)) %>%
    ungroup()
  
  return(resultado)
}


calcular_votos_coalicion_2018 <- function(datos_2015) {
  
  # Función auxiliar para determinar si un partido es coalición
  es_coalicion <- function(partido) {
    # No es coalición si:
    # 1. Es "NVA_ALIANZA" (partido individual)
    # 2. Comienza con "CAND_IND" seguido de número
    # 3. No contiene "_"
    
    if (is.na(partido)) return(FALSE)
    
    # Casos especiales que NO son coaliciones
    if (partido == "NVA_ALIANZA") return(FALSE)
    if (str_detect(partido, "^CAND_IND\\d+$")) return(FALSE)
    
    # Es coalición si contiene "_"
    return(str_detect(partido, "_"))
  }
  
  # Función auxiliar para obtener partidos componentes de una coalición
  obtener_partidos_componentes <- function(coalicion) {
    if (is.na(coalicion)) return(character(0))
    return(str_split(coalicion, "_")[[1]])
  }
  
  # Función auxiliar para calcular votos de coalición en un distrito específico
  calcular_votos_distrito <- function(datos_distrito) {
    
    # Crear diccionario de votos por partido en este distrito
    votos_por_partido <- setNames(datos_distrito$Votos, datos_distrito$Partido)
    
    # Calcular votos_coalicion para cada fila
    datos_distrito$votos_coalicion <- sapply(1:nrow(datos_distrito), function(i) {
      partido <- datos_distrito$Partido[i]
      votos_originales <- datos_distrito$Votos[i]
      
      # Si no es coalición, usar votos originales
      if (!es_coalicion(partido)) {
        return(ifelse(is.na(votos_originales), 0, votos_originales))
      }
      
      # Si es coalición, sumar votos de partidos componentes más votos propios
      partidos_componentes <- obtener_partidos_componentes(partido)
      
      # Sumar votos de cada partido componente
      votos_componentes <- sum(sapply(partidos_componentes, function(p) {
        voto <- votos_por_partido[p]
        return(ifelse(is.na(voto), 0, voto))
      }), na.rm = TRUE)
      
      # Sumar votos propios de la coalición (si existen)
      votos_propios <- ifelse(is.na(votos_originales), 0, votos_originales)
      
      return(votos_componentes + votos_propios)
    })
    
    return(datos_distrito)
  }
  
  # Aplicar el cálculo agrupado por ID_ESTADO, ID_DISTRITO y CABECERA_DISTRITAL
  resultado <- datos_2015 %>%
    group_by(ID_ESTADO, NOMBRE_ESTADO, ID_DISTRITO_LOCAL, CABECERA_DISTRITAL_LOCAL) %>%
    group_modify(~ calcular_votos_distrito(.x)) %>%
    ungroup()
  
  return(resultado)
}