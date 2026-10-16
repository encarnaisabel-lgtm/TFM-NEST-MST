# =========================================================
# Script2_MST_RegionesReales_3Paneles.R
# MST de regiones reales + poda fija + probabilidad
# =========================================================

rm(list = ls())

# -----------------------------
# 1) Librería
# -----------------------------
library(dplyr)

# -----------------------------
# 2) Funciones auxiliares
# -----------------------------
source("funcionesAuxiliares.R")

calcularProbConservacion <- function(dn, p_max = 0.5) {
  prob <- rep(0, length(dn))
  ok <- is.finite(dn) & dn <= p_max
  prob[ok] <- 1 - dn[ok] / p_max
  prob[prob < 0] <- 0
  prob[prob > 1] <- 1
  prob
}

clasificarProb5 <- function(prob) {
  cut(
    prob * 100,
    breaks = c(0, 20, 40, 60, 80, 100),
    include.lowest = TRUE,
    right = TRUE,
    labels = c("0-20", "20-40", "40-60", "60-80", "80-100")
  )
}

colorGrisClase <- function(clase) {
  mapa <- c(
    "0-20" = "#FFFAAC",
    "20-40" = "#E0D28E",
    "40-60" = "#AFA995",
    "60-80" = "#3F6FA8",
    "80-100" = "#001C3F"
  )
  unname(mapa[as.character(clase)])
}

anadirCoordenadasAristas <- function(mst_df, centroides_df) {
  if (is.null(mst_df) || nrow(mst_df) == 0) return(mst_df)
  
  mst_df <- mst_df %>%
    left_join(centroides_df, by = c("v1" = "iNest")) %>%
    rename(x1 = x, y1 = y) %>%
    left_join(centroides_df, by = c("v2" = "iNest")) %>%
    rename(x2 = x, y2 = y)
  
  mst_df
}

# -----------------------------
# 3) Archivo de entrada
# -----------------------------
archivo_region <- "Muestra_Ind_Regions/MonR2_NestClass.csv"

region <- sub("_NestClass$", "", tools::file_path_sans_ext(basename(archivo_region)))
tipo <- "Real"

# -----------------------------
# 4) Carpeta de salida
# -----------------------------
path_salida <- "Resultados_Script2_Reales"
if (!dir.exists(path_salida)) dir.create(path_salida, recursive = TRUE)

# -----------------------------
# 5) Leer datos
# -----------------------------
datos <- read.csv(archivo_region, stringsAsFactors = FALSE) %>%
  select(RAJ2000, DEJ2000, NEST)

datos_nest <- datos %>%
  filter(NEST != 0)

# -----------------------------
# 6) Centroides
# -----------------------------
centroides <- datos_nest %>%
  group_by(NEST) %>%
  summarise(
    x = mean(RAJ2000),
    y = mean(DEJ2000),
    .groups = "drop"
  ) %>%
  rename(iNest = NEST)

# -----------------------------
# 7) Distancias estrella-estrella
# -----------------------------
coords <- as.matrix(datos_nest[, c("RAJ2000", "DEJ2000")])
dist_mat <- as.matrix(dist(coords))

# -----------------------------
# 8) Distancias entre NESTs
# -----------------------------
dist_nests <- distClust(
  distMat = dist_mat,
  clustInd = datos_nest$NEST
)

# -----------------------------
# 9) MST
# -----------------------------
mst <- MstDist(dist_nests)
names(mst) <- c("v1", "v2", "d")

# -----------------------------
# 10) Envolvente convexa y radio equivalente
# -----------------------------
coords_region <- as.matrix(datos[, c("RAJ2000", "DEJ2000")])

hull <- chull(coords_region[,1], coords_region[,2])
hull <- c(hull, hull[1])
hc <- coords_region[hull, ]

area_region <- 0.5 * abs(sum(hc[-1,1] * hc[-nrow(hc),2] - hc[-nrow(hc),1] * hc[-1,2]))
radio_eq <- sqrt(area_region / pi)

# -----------------------------
# 11) Normalización
# -----------------------------
mst <- mst %>%
  mutate(
    radio_eq = radio_eq,
    dn = d / radio_eq,
    region = region,
    tipo = tipo
  )

# -----------------------------
# 12) Guardar tablas
# -----------------------------
write.csv(
  centroides,
  file.path(path_salida, paste0("centroides_", region, ".csv")),
  row.names = FALSE
)

write.csv(
  mst,
  file.path(path_salida, paste0("mst_normalizado_", region, ".csv")),
  row.names = FALSE
)

# -----------------------------
# 13) Preparar tres versiones del árbol
# -----------------------------
mst_original_xy <- anadirCoordenadasAristas(mst, centroides)

mst_poda_xy <- mst %>%
  filter(dn <= 0.25) %>%
  anadirCoordenadasAristas(centroides)

mst_prob <- mst %>%
  mutate(
    prob_conservacion = calcularProbConservacion(dn, p_max = 0.5),
    clase_prob = clasificarProb5(prob_conservacion),
    col_prob = colorGrisClase(clase_prob)
  )

mst_prob_xy <- anadirCoordenadasAristas(mst_prob, centroides)

# -----------------------------
# 14) PDF con 3 paneles
# -----------------------------
graphics.off()



pdf(
  file.path(path_salida, paste0("Comparacion_MST_", region, ".pdf")),
  width = 15,
  height = 5
)

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot(
  datos$RAJ2000, datos$DEJ2000,
  pch = 18, cex = 0.45, 
  col = "grey70",
  xlim = rev(range(datos$RAJ2000)),
  ylim= range(datos$DEJ2000),
  xlab = "RAJ2000", ylab = "DEJ2000",
  main = paste(region, "- MST")
)

points(centroides$x, centroides$y, pch = 19, cex = 0.8)

if (nrow(mst_original_xy) > 0) {
  for (i in seq_len(nrow(mst_original_xy))) {
    segments(
      mst_original_xy$x1[i], mst_original_xy$y1[i],
      mst_original_xy$x2[i], mst_original_xy$y2[i],
      lwd = 1.5, col = "black"
    )
  }
}

plot(
  datos$RAJ2000, datos$DEJ2000,
  pch = 16, cex = 0.45, 
  col = "grey70",
  xlim = rev(range(datos$RAJ2000)),
  ylim= range(datos$DEJ2000),
  xlab = "RAJ2000", ylab = "DEJ2000",
  main = "Poda: dn <= 0.25"
)

points(centroides$x, centroides$y, pch = 19, cex = 0.8)

if (nrow(mst_poda_xy) > 0) {
  for (i in seq_len(nrow(mst_poda_xy))) {
    segments(
      mst_poda_xy$x1[i], mst_poda_xy$y1[i],
      mst_poda_xy$x2[i], mst_poda_xy$y2[i],
      lwd = 2, col = "black"
    )
  }
}

plot(
  datos$RAJ2000, datos$DEJ2000,
  pch = 18, cex = 0.45, 
  col = "grey70",
  xlim = rev(range(datos$RAJ2000)),
  ylim= range(datos$DEJ2000),
  xlab = "RAJ2000", ylab = "DEJ2000",
  main = "Probabilidad de conservación"
)

points(centroides$x, centroides$y, pch = 19, cex = 0.8)

if (nrow(mst_prob_xy) > 0) {
  for (i in seq_len(nrow(mst_prob_xy))) {
    segments(
      mst_prob_xy$x1[i], mst_prob_xy$y1[i],
      mst_prob_xy$x2[i], mst_prob_xy$y2[i],
      lwd = 2.6, col = mst_prob_xy$col_prob[i]
    )
  }
}

legend(
  "topright",
  legend = c("80-100", "60-80", "40-60", "20-40", "0-20"),
  col = c("#001C3F", "#3F6FA8", "#AFA995", "#E0D28E", "#FFFAAC"),
  lwd = 3, bty = "n", cex = 0.8,
  title = "Prob. (%)"
)

dev.off()

# -----------------------------
# 15) Resumen
# -----------------------------
cat("\n=============================\n")
cat("Región:", region, "\n")
cat("Tipo:", tipo, "\n")
cat("Nº estrellas:", nrow(datos), "\n")
cat("Nº estrellas en NESTs:", nrow(datos_nest), "\n")
cat("Nº NESTs:", nrow(centroides), "\n")
cat("Radio equivalente:", radio_eq, "\n")
cat("=============================\n")