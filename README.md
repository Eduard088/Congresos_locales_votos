# Análisis de Procesos Electorales en los Congresos Locales de México (2015-2023)

## Descripción del Proyecto

Este proyecto se centra en la limpieza y el análisis de datos de los procesos electorales en los congresos locales de México entre 2015 y 2023, en concreto incluye el universo de las votaciones por todas las candidaturas postuladas en los legislativos locales, en todos los estados. La estructura del proyecto está diseñada para facilitar la organización, procesamiento y visualización de datos utilizando R y Quarto.

## Estructura del Proyecto

```
.
├── models/                   # Contiene los modelos y scripts relacionados
│   ├── R/                    # Carpeta para scripts en R
│   │   ├── script.R          # Script para entrenar modelos o realizar análisis
│
├── {{cookiecutter.project_slug}}/  # Carpeta principal del proyecto
│   ├── data/                 # Datos utilizados en el proyecto
│   │   ├── raw/              # Datos originales sin procesar
│   │   ├── preprocessed/     # Datos con transformaciones iniciales
│   │   ├── processed/        # Datos limpios y estructurados
│   │   ├── final/            # Datos finales listos para análisis o modelado
│   │
│   ├── notebooks/            # Notebooks de Quarto con los análisis
│   │   ├── congresos/        # Notebooks específicos sobre congresos locales
│   │   │   ├── 01_transformacion_datos.qmd
│   │   │   ├── 02_transformacion_datos.qmd
│   │   │   ├── 03_transformacion_datos.qmd
│   │
└── README.md                 # Documentación del proyecto
```

## Requisitos

- R (versión 4.0 o superior)
- Paquetes de R: `tidyverse`, `ggplot2`, `dplyr`, `readr`, `janitor`, `quarto`

## Instalación

1. Clona este repositorio:
    ```sh
    git clone https://github.com/tu_usuario/tu_repositorio.git
    ```
2. Instala los paquetes necesarios en R:
    ```r
    install.packages(c("tidyverse", "ggplot2", "dplyr", "readr", "janitor", "quarto"))
    ```

## Uso

### Limpieza y Transformación de Datos

Ejecuta los notebooks de Quarto en orden para realizar la limpieza y transformación de los datos:

```sh
quarto render notebooks/congresos/01_transformacion_datos.qmd
quarto render notebooks/congresos/02_transformacion_datos.qmd
quarto render notebooks/congresos/03_transformacion_datos.qmd
```

### Análisis y Modelado en R

Para realizar el análisis y modelado en R, ejecuta el siguiente script:

```r
source("models/R/script.R")
```

## Contribuciones

Las contribuciones son bienvenidas. Para reportar errores o sugerencias, abre un issue o envía un pull request en el repositorio.

## Licencia

Este proyecto está bajo la Licencia MIT. Consulta el archivo `LICENSE` para más detalles.

## Contacto

Para cualquier consulta, puedes escribirme a: **[eduardibareapoot@outlook.com]**