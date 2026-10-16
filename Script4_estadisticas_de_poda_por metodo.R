# =========================================================
# Script4a_Estadisticas_poda.R
# Estadísticas de los resultados de poda
# =========================================================

rm(list = ls())

library(ggplot2)
library(dplyr)


# ---------------------------------------------------------
# 1. Carpeta de resultados que se quiere analizar
# ---------------------------------------------------------

# Escribe la carpeta correspondiente al método de poda

path_resultados <- "Resultados_Script3_poda_iqr"


# ---------------------------------------------------------
# 2. Leer resultados de poda
# ---------------------------------------------------------

datos <- read.csv(
  file.path(
    path_resultados,
    "resumen_poda_por_region.csv"
  ),
  stringsAsFactors = FALSE
)


# ---------------------------------------------------------
# 3. Nombres de las distribuciones
# ---------------------------------------------------------

datos$distribucion <- datos$tipo

datos$distribucion[
  datos$tipo == "Structs_frac1p6"
] <- "Fractal D=1.6"

datos$distribucion[
  datos$tipo == "Structs_frac2p0"
] <- "Fractal D=2.0"

datos$distribucion[
  datos$tipo == "Structs_frac2p6"
] <- "Fractal D=2.6"

datos$distribucion[
  datos$tipo == "Structs_frac3p0"
] <- "Fractal D=3.0"

datos$distribucion[
  datos$tipo == "Structs_Radial_e0"
] <- "Radial E=0"

datos$distribucion[
  datos$tipo == "Structs_Radial_e1"
] <- "Radial E=1"

datos$distribucion[
  datos$tipo == "Structs_Radial_e2"
] <- "Radial E=2"

datos$distribucion[
  datos$tipo == "Structs_Radial_e2p5"
] <- "Radial E=2.5"

datos$distribucion[
  datos$tipo == "Structs_Plummer_a0.2"
] <- "Plummer a=0.2"


# ---------------------------------------------------------
# 4. Orden de las distribuciones
# ---------------------------------------------------------

orden_distribuciones <- c(
  "Fractal D=1.6",
  "Fractal D=2.0",
  "Fractal D=2.6",
  "Fractal D=3.0",
  "Radial E=0",
  "Radial E=1",
  "Radial E=2",
  "Radial E=2.5",
  "Plummer a=0.2"
)

datos$distribucion <- factor(
  datos$distribucion,
  levels = orden_distribuciones
)


# Ordenar por distribución.
# Dentro de cada distribución se mantiene
# el orden original de las regiones.

datos <- datos %>%
  arrange(distribucion)

datos$region <- factor(
  datos$region,
  levels = unique(datos$region)
)


# ---------------------------------------------------------
# 5. Método de poda
# ---------------------------------------------------------

metodo <- unique(datos$criterio)

if (length(metodo) > 1) {
  metodo <- "varios métodos"
}


# ---------------------------------------------------------
# 6. GRÁFICO
# Número de ramas eliminadas por región
# ---------------------------------------------------------

grafico_region <- ggplot(
  datos,
  aes(
    x = region,
    y = n_aristas_eliminadas
  )
) +
  
  geom_col(
    fill = "steelblue"
  ) +
  
  scale_x_discrete(
    labels = function(x) {
      sub("^Structs_", "", x)
    }
  ) +
  
  theme_minimal() +
  
  labs(
    title = paste(
      "Aristas eliminadas por región. Método:",
      metodo
    ),
    x = "Región",
    y = "Nº de aristas eliminadas"
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


print(grafico_region)


ggsave(
  file.path(
    path_resultados,
    "grafico_poda_por_region.pdf"
  ),
  grafico_region,
  width = 12,
  height = 6
)


# ---------------------------------------------------------
# 7. Fracción media eliminada por distribución
# ---------------------------------------------------------

resumen_distribucion <- datos %>%
  group_by(distribucion) %>%
  summarise(
    fraccion_eliminada_media =
      mean(
        fraccion_eliminada,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


# ---------------------------------------------------------
# 8. Guardar resumen por distribución
# ---------------------------------------------------------

write.csv(
  resumen_distribucion,
  file.path(
    path_resultados,
    "resumen_poda_por_tipo.csv"
  ),
  row.names = FALSE
)


cat(
  "Estadísticas de poda finalizadas.\n",
  "Método analizado:",
  metodo,
  "\n"
)