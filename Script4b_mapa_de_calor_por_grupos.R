# ============================================================
# Script4b_Mapas_de_calor_por_grupo.R
#
# MAPA DE CALOR DE LOS MÉTODOS DE PODA POR GRUPO ESTRUCTURAL
#
# Se representa la MEDIA de la columna "fraccion_eliminada"
# para cada grupo estructural y cada método de poda.
#
# fraccion_eliminada ya viene calculada en los archivos:
#
# fraccion_eliminada =
# ramas eliminadas / ramas totales de la región
#
# ============================================================


# ------------------------------------------------------------
# 1. Cargar paquetes
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)


# ------------------------------------------------------------
# 2. Rutas de los archivos
# ------------------------------------------------------------

# ------------------------------------------------------------
# 2. Rutas de los archivos
# ------------------------------------------------------------

archivo_iqr <-
  "Resultados_Script3_poda_IQR/resumen_poda_por_region.csv"

archivo_p90 <-
  "Resultados_Script3_poda_p90/resumen_poda_por_region.csv"

archivo_p95 <-
  "Resultados_Script3_poda_p95/resumen_poda_por_region.csv"

archivo_media_sd <-
  "Resultados_Script3_poda_media_sd/resumen_poda_por_region.csv"

archivo_dn_fijo <-
  "Resultados_Script3_poda_dn_fijo/resumen_poda_por_region.csv"

archivo_dn_fijo_0p25 <-
  "Resultados_Script3_poda_dn_fijo_0p25/resumen_poda_por_region.csv"

archivo_alfa_dn_max <-
  "Resultados_Script3_poda_alfa_dn_max/resumen_poda_por_region.csv"

archivo_codo <-
  "Resultados_Script3_poda_codo/resumen_poda_por_region.csv"

# ------------------------------------------------------------
# 3. Leer los archivos
# ------------------------------------------------------------

datos_iqr <- read.csv(archivo_iqr)
datos_p90 <- read.csv(archivo_p90)
datos_p95 <- read.csv(archivo_p95)
datos_media_sd <- read.csv(archivo_media_sd)
datos_dn_fijo <- read.csv(archivo_dn_fijo)
datos_dn_fijo_0p25 <- read.csv(archivo_dn_fijo_0p25)
datos_alfa_dn_max <- read.csv(archivo_alfa_dn_max)
datos_codo <- read.csv(archivo_codo)


# ------------------------------------------------------------
# 4. Añadir una columna para indicar el método de poda
# ------------------------------------------------------------

datos_iqr$metodo <- "IQR"
datos_p90$metodo <- "P90"
datos_p95$metodo <- "P95"
datos_media_sd$metodo <- "Media + SD"
datos_dn_fijo$metodo <- "dn=0.5"
datos_dn_fijo_0p25$metodo <- "dn=0.25"
datos_alfa_dn_max$metodo <- "α · dn(max)"
datos_codo$metodo <- "codo"


# ------------------------------------------------------------
# 5. Unir los archivos
# ------------------------------------------------------------

datos <- bind_rows(
  datos_iqr,
  datos_p90,
  datos_p95,
  datos_media_sd,
  datos_dn_fijo,
  datos_dn_fijo_0p25,
  datos_alfa_dn_max,
  datos_codo
)


# ------------------------------------------------------------
# 6. Cambiar los nombres de las distribuciones
#    para que aparezcan más claros
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# 7. Agrupar las distribuciones
# ------------------------------------------------------------

datos$grupo <- NA


# Grupo 1:
# Fractales fuertemente subestructuradas

datos$grupo[
  datos$tipo %in% c(
    "Structs_frac1p6",
    "Structs_frac2p0"
  )
] <- "Grupo 1"


# Grupo 2:
# Distribuciones levemente estructuradas y homogéneas

datos$grupo[
  datos$tipo %in% c(
    "Structs_frac2p6",
    "Structs_frac3p0",
    "Structs_Radial_e0",
    "Structs_Radial_e1"
  )
] <- "Grupo 2"


# Grupo 3:
# Distribuciones concentradas

datos$grupo[
  datos$tipo %in% c(
    "Structs_Radial_e2",
    "Structs_Radial_e2p5"
  )
] <- "Grupo 3"


# Grupo 4:
# Concentración central

datos$grupo[
  datos$tipo == "Structs_Plummer_a0.2"
] <- "Grupo 4"


# ------------------------------------------------------------
# 8. Calcular la MEDIA de la fracción eliminada por grupo
#
# IMPORTANTE:
#
# Aquí NO se recalcula fraccion_eliminada.
#
# Se utiliza directamente la columna "fraccion_eliminada"
# de cada región y se calcula su media.

# ------------------------------------------------------------

resumen <- datos %>%
  group_by(grupo, metodo) %>%
  summarise(
    fraccion_media_eliminada =
      mean(fraccion_eliminada, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 9. Ver los resultados numéricos
# ------------------------------------------------------------

print(resumen)


# ------------------------------------------------------------
# 10. Guardar los resultados en un CSV
# ------------------------------------------------------------

write.csv(
  resumen,
  "resumen_comparacion_metodos_poda.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 11. Orden de los métodos
# ------------------------------------------------------------

datos_metodos <- c(
  "IQR",
  "P90",
  "P95",
  "Media + SD",
  "dn=0.5",
  "dn=0.25",
  "α · dn(max)",
  "codo"
)

resumen$metodo <- factor(
  resumen$metodo,
  levels = datos_metodos
)


# ------------------------------------------------------------
# 12. Orden de los grupos
# ------------------------------------------------------------

orden_grupos <- c(
  "Grupo 1",
  "Grupo 2",
  "Grupo 3",
  "Grupo 4"
)

resumen$grupo <- factor(
  resumen$grupo,
  levels = rev(orden_grupos)
)


# ------------------------------------------------------------
# 13. Crear el mapa de calor
# ------------------------------------------------------------

mapa_calor <- ggplot(
  resumen,
  aes(
    x = metodo,
    y = grupo,
    fill = fraccion_media_eliminada
  )
) +
  
  geom_tile(
    colour = "white",
    linewidth = 1
  ) +
  
  # Escribir el porcentaje dentro de cada celda
  geom_text(
    aes(
      label = percent(
        fraccion_media_eliminada,
        accuracy = 1
      )
    ),
    size = 4
  ) +
  
  # Escala de color:
  # blanco = poca poda
  # oscuro = mucha poda
  scale_fill_gradient(
    low = "white",
    high = "#B22222",
    limits = c(0, 1),
    labels = percent_format(
      accuracy = 1
    ),
    name = "Fracción media\nde ramas eliminadas"
  ) +
  
  labs(
    x = "Método de poda",
    y = "Grupo estructural"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      colour = "black"
    ),
    
    axis.text.y = element_text(
      colour = "black"
    ),
    
    axis.title = element_text(
      face = "bold"
    )
  )


# ------------------------------------------------------------
# 14. Mostrar el mapa de calor
# ------------------------------------------------------------

print(mapa_calor)


# ------------------------------------------------------------
# 15. Guardar el mapa de calor
# ------------------------------------------------------------

ggsave(
  "mapa_calor_metodos_poda.png",
  mapa_calor,
  width = 9,
  height = 6,
  dpi = 300
)


ggsave(
  "mapa_calor_metodos_poda.pdf",
  mapa_calor,
  width = 9,
  height = 6
)