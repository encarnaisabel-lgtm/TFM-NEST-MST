# =========================================================
# Script7_Comparacion.R
# Comparación entre:
#   1) MST original
#   2) Poda fija dn <= 0.25
#   3) Poda probabilística
# =========================================================

rm(list = ls())

library(dplyr)


# ---------------------------------------------------------
# 1. Rutas
# ---------------------------------------------------------

path_datos <- "Datos"
path_script1 <- "Resultados_Script1"
path_script2 <- "Resultados_Script2"

# Resultados de la poda fija dn = 0.25
path_script3 <- "Resultados_Script3_dn_fijo"

# Resultados de la poda probabilística
path_script6 <- "Resultados_Script6"

path_resultados <- "Resultados_Script7_Comparacion"

if (!dir.exists(path_resultados)) {
  dir.create(path_resultados)
}

archivo_pdf <- file.path(
  path_resultados,
  "Comparacion_MST_original_dn025_probabilidad.pdf"
)


# ---------------------------------------------------------
# 2. Leer resultados de los scripts anteriores
# ---------------------------------------------------------

# MST original normalizado
mst_original <- read.csv(
  file.path(
    path_script2,
    "mst_normalizados_global.csv"
  ),
  stringsAsFactors = FALSE
)

# MST podado con dn <= 0.25
mst_fijo <- read.csv(
  file.path(
    path_script3,
    "mst_normalizados_podado_global.csv"
  ),
  stringsAsFactors = FALSE
)

# MST con probabilidad de conservación
mst_prob <- read.csv(
  file.path(
    path_script6,
    "mst_probabilidad_ramas.csv"
  ),
  stringsAsFactors = FALSE
)


# ---------------------------------------------------------
# 3. Nombres de las distribuciones
# ---------------------------------------------------------

mst_original$distribucion <- mst_original$tipo

mst_original$distribucion[
  mst_original$tipo == "Structs_frac1p6"
] <- "Fractal D=1.6"

mst_original$distribucion[
  mst_original$tipo == "Structs_frac2p0"
] <- "Fractal D=2.0"

mst_original$distribucion[
  mst_original$tipo == "Structs_frac2p6"
] <- "Fractal D=2.6"

mst_original$distribucion[
  mst_original$tipo == "Structs_frac3p0"
] <- "Fractal D=3.0"

mst_original$distribucion[
  mst_original$tipo == "Structs_Radial_e0"
] <- "Radial E=0"

mst_original$distribucion[
  mst_original$tipo == "Structs_Radial_e1"
] <- "Radial E=1"

mst_original$distribucion[
  mst_original$tipo == "Structs_Radial_e2"
] <- "Radial E=2"

mst_original$distribucion[
  mst_original$tipo == "Structs_Radial_e2p5"
] <- "Radial E=2.5"

mst_original$distribucion[
  mst_original$tipo == "Structs_Plummer_a0.2"
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

mst_original$distribucion <- factor(
  mst_original$distribucion,
  levels = orden_distribuciones
)

mst_original <- mst_original %>%
  arrange(distribucion)


# Mantener el orden original de las regiones
regiones <- unique(
  mst_original$region
)


# ---------------------------------------------------------
# 5. Colores de la poda probabilística
# ---------------------------------------------------------

colores_prob <- c(
  "0-20" = "#FFFAAC",
  "20-40" = "#E0D28E",
  "40-60" = "#AFA995",
  "60-80" = "#3F6FA8",
  "80-100" = "#001C3F"
)


# ---------------------------------------------------------
# 6. Abrir PDF
# ---------------------------------------------------------

pdf(
  archivo_pdf,
  width = 15,
  height = 5
)


# ---------------------------------------------------------
# 7. Procesar cada región
# ---------------------------------------------------------

for (region in regiones) {
  
  cat("Procesando:", region, "\n")
  
  
  # -------------------------------------------------------
  # Seleccionar datos de la región
  # -------------------------------------------------------
  
  mst_region_original <- mst_original[
    mst_original$region == region,
  ]
  
  mst_region_fijo <- mst_fijo[
    mst_fijo$region == region,
  ]
  
  mst_region_prob <- mst_prob[
    mst_prob$region == region,
  ]
  
  
  # -------------------------------------------------------
  # Leer estrellas y centroides
  # -------------------------------------------------------
  
  stars <- read.csv(
    file.path(
      path_datos,
      paste0(
        region,
        ".csv"
      )
    ),
    stringsAsFactors = FALSE
  )
  
  centroides <- read.csv(
    file.path(
      path_script1,
      paste0(
        "centroides_",
        region,
        ".csv"
      )
    ),
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # Coordenadas del MST original
  # -------------------------------------------------------
  
  i1_original <- match(
    mst_region_original$v1,
    centroides$iNest
  )
  
  i2_original <- match(
    mst_region_original$v2,
    centroides$iNest
  )
  
  
  # -------------------------------------------------------
  # Coordenadas del MST con poda fija
  # -------------------------------------------------------
  
  i1_fijo <- match(
    mst_region_fijo$v1,
    centroides$iNest
  )
  
  i2_fijo <- match(
    mst_region_fijo$v2,
    centroides$iNest
  )
  
  
  # -------------------------------------------------------
  # Coordenadas del MST probabilístico
  # -------------------------------------------------------
  
  i1_prob <- match(
    mst_region_prob$v1,
    centroides$iNest
  )
  
  i2_prob <- match(
    mst_region_prob$v2,
    centroides$iNest
  )
  
  
  # -------------------------------------------------------
  # Tres paneles
  # -------------------------------------------------------
  
  par(
    mfrow = c(1, 3),
    mar = c(4, 4, 3, 1)
  )
  
  
  # -------------------------------------------------------
  # PANEL 1: MST original
  # -------------------------------------------------------
  
  plot(
    stars$ra,
    stars$dec,
    pch = 16,
    cex = 0.45,
    col = "grey70",
    asp = 1,
    xlab = "RA",
    ylab = "DEC",
    xlim = rev(
      range(
        stars$ra,
        na.rm = TRUE
      )
    ),
    main = paste(
      region,
      "- Original"
    )
  )
  
  
  for (i in seq_len(nrow(mst_region_original))) {
    
    segments(
      centroides$x[i1_original[i]],
      centroides$y[i1_original[i]],
      centroides$x[i2_original[i]],
      centroides$y[i2_original[i]],
      lwd = 1.2
    )
  }
  
  
  points(
    centroides$x,
    centroides$y,
    pch = 19,
    cex = 0.8
  )
  
  
  # -------------------------------------------------------
  # PANEL 2: poda fija dn <= 0.25
  # -------------------------------------------------------
  
  plot(
    stars$ra,
    stars$dec,
    pch = 16,
    cex = 0.45,
    col = "grey70",
    asp = 1,
    xlab = "RA",
    ylab = "DEC",
    xlim = rev(
      range(
        stars$ra,
        na.rm = TRUE
      )
    ),
    main = "Poda: dn <= 0.25"
  )
  
  
  if (nrow(mst_region_fijo) > 0) {
    
    for (i in seq_len(nrow(mst_region_fijo))) {
      
      segments(
        centroides$x[i1_fijo[i]],
        centroides$y[i1_fijo[i]],
        centroides$x[i2_fijo[i]],
        centroides$y[i2_fijo[i]],
        lwd = 2
      )
    }
  }
  
  
  points(
    centroides$x,
    centroides$y,
    pch = 19,
    cex = 0.8
  )
  
  
  # -------------------------------------------------------
  # PANEL 3: poda probabilística
  # -------------------------------------------------------
  
  plot(
    stars$ra,
    stars$dec,
    pch = 16,
    cex = 0.45,
    col = "grey70",
    asp = 1,
    xlab = "RA",
    ylab = "DEC",
    xlim = rev(
      range(
        stars$ra,
        na.rm = TRUE
      )
    ),
    main = "Probabilidad de conservación"
  )
  
  
  for (i in seq_len(nrow(mst_region_prob))) {
    
    clase <- as.character(
      mst_region_prob$clase_probabilidad[i]
    )
    
    segments(
      centroides$x[i1_prob[i]],
      centroides$y[i1_prob[i]],
      centroides$x[i2_prob[i]],
      centroides$y[i2_prob[i]],
      col = colores_prob[clase],
      lwd = 2.5
    )
  }
  
  
  points(
    centroides$x,
    centroides$y,
    pch = 19,
    cex = 0.8
  )
  
  
  legend(
    "topright",
    legend = c(
      "80-100 %",
      "60-80 %",
      "40-60 %",
      "20-40 %",
      "0-20 %"
    ),
    col = c(
      "#001C3F",
      "#3F6FA8",
      "#AFA995",
      "#E0D28E",
      "#FFFAAC"
    ),
    lwd = 3,
    bty = "n",
    cex = 0.8,
    title = "Probabilidad"
  )
}


# ---------------------------------------------------------
# 8. Cerrar PDF
# ---------------------------------------------------------

dev.off()


cat(
  "Script7 finalizado.\n",
  "Documento generado en:\n",
  archivo_pdf,
  "\n"
)