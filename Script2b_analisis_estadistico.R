# =========================================================
# Script2b_analisis_estadistico.R
# Análisis estadístico de las longitudes normalizadas
# generadas por Script2_MST_normalizado.R
# =========================================================

rm(list = ls())

library(ggplot2)

# ---------------------------------------------------------
# 1. Rutas
# ---------------------------------------------------------

path_resultados <- "Resultados_Script2b"

archivo_mst <- file.path(
  path_resultados,
  "mst_normalizados_global.csv"
)

if (!file.exists(archivo_mst)) {
  stop("No se encuentra mst_normalizados_global.csv")
}


# ---------------------------------------------------------
# 2. Leer resultados de Script2
# ---------------------------------------------------------

mst_normalizados <- read.csv(
  archivo_mst,
  stringsAsFactors = FALSE
)

if (!all(c("dn", "tipo") %in% names(mst_normalizados))) {
  stop("El archivo debe contener las columnas 'dn' y 'tipo'")
}


# ---------------------------------------------------------
# 3. Orden de los tipos estructurales
# ---------------------------------------------------------

orden_tipos <- c(
  "Structs_frac1p6",
  "Structs_frac2p0",
  "Structs_frac2p6",
  "Structs_frac3p0",
  "Structs_Radial_e0",
  "Structs_Radial_e1",
  "Structs_Radial_e2",
  "Structs_Radial_e2p5",
  "Structs_Plummer_a0.2"
)

mst_normalizados$tipo <- factor(
  mst_normalizados$tipo,
  levels = orden_tipos
)


# ---------------------------------------------------------
# 4. Grupos estructurales
# ---------------------------------------------------------

mst_normalizados$grupo <- NA

mst_normalizados$grupo[
  mst_normalizados$tipo %in% c(
    "Structs_frac1p6",
    "Structs_frac2p0"
  )
] <- "Grupo 1: Fuertemente subestructuradas - Subestructuradas"

mst_normalizados$grupo[
  mst_normalizados$tipo %in% c(
    "Structs_frac2p6",
    "Structs_frac3p0",
    "Structs_Radial_e0",
    "Structs_Radial_e1"
  )
] <- "Grupo 2: Levemente estructuradas y homogéneas"

mst_normalizados$grupo[
  mst_normalizados$tipo %in% c(
    "Structs_Radial_e2",
    "Structs_Radial_e2p5"
  )
] <- "Grupo 3: Concentradas y fuertemente concentradas"

mst_normalizados$grupo[
  mst_normalizados$tipo == "Structs_Plummer_a0.2"
] <- "Grupo 4: Concentración central"


orden_grupos <- c(
  "Grupo 1: Fuertemente subestructuradas - Subestructuradas",
  "Grupo 2: Levemente estructuradas y homogéneas",
  "Grupo 3: Concentradas y fuertemente concentradas",
  "Grupo 4: Concentración central"
)

mst_normalizados$grupo <- factor(
  mst_normalizados$grupo,
  levels = orden_grupos
)


colores_grupo <- c(
  "Grupo 1: Fuertemente subestructuradas - Subestructuradas" = "#0070FF54",
  "Grupo 2: Levemente estructuradas y homogéneas" = "#44AAFF4E",
  "Grupo 3: Concentradas y fuertemente concentradas" = "#FFAA5550",
  "Grupo 4: Concentración central" = "#A051144D"
)


# ---------------------------------------------------------
# 6. Estadísticos descriptivos por tipo
# ---------------------------------------------------------

resumen_dn_por_tipo <- do.call(
  rbind,
  lapply(
    split(mst_normalizados$dn, mst_normalizados$tipo),
    function(x) {
      
      data.frame(
        n = length(x),
        media = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        minimo = min(x, na.rm = TRUE),
        p25 = unname(quantile(x, 0.25, na.rm = TRUE)),
        mediana = unname(quantile(x, 0.50, na.rm = TRUE)),
        p75 = unname(quantile(x, 0.75, na.rm = TRUE)),
        p90 = unname(quantile(x, 0.90, na.rm = TRUE)),
        p95 = unname(quantile(x, 0.95, na.rm = TRUE)),
        maximo = max(x, na.rm = TRUE)
      )
    }
  )
)

resumen_dn_por_tipo$tipo <- rownames(resumen_dn_por_tipo)
rownames(resumen_dn_por_tipo) <- NULL

resumen_dn_por_tipo <- resumen_dn_por_tipo[
  ,
  c(
    "tipo",
    "n",
    "media",
    "sd",
    "minimo",
    "p25",
    "mediana",
    "p75",
    "p90",
    "p95",
    "maximo"
  )
]

write.csv(
  resumen_dn_por_tipo,
  file.path(
    path_resultados,
    "resumen_dn_por_tipo.csv"
  ),
  row.names = FALSE
)


# ---------------------------------------------------------
# 7. Conteo de aristas por tipo
# ---------------------------------------------------------

conteo_aristas_por_tipo <- as.data.frame(
  table(mst_normalizados$tipo)
)

names(conteo_aristas_por_tipo) <- c(
  "tipo",
  "n_aristas"
)

write.csv(
  conteo_aristas_por_tipo,
  file.path(
    path_resultados,
    "conteo_aristas_por_tipo.csv"
  ),
  row.names = FALSE
)


# ---------------------------------------------------------
# 8. Densidad por grupos estructurales
# ---------------------------------------------------------

fig_densidad <- ggplot(
  mst_normalizados,
  aes(
    x = dn,
    fill = grupo
  )
) +
  geom_density(
    colour = "black",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    values = colores_grupo
  ) +
  labs(
    x = "dn = d / r",
    y = "Densidad",
    fill = "Grupo estructural",
    title = "Distribución de densidad de dn por grupos estructurales"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(
    path_resultados,
    "densidad_dn_por_grupo.pdf"
  ),
  plot = fig_densidad,
  width = 8,
  height = 6
)


cat("Análisis estadístico finalizado.\n")