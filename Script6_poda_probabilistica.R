# =========================================================
# Script6_Poda_probabilistica.R
# Probabilidad de conservación de las ramas del MST
# =========================================================

rm(list = ls())


# ---------------------------------------------------------
# 1. Rutas
# ---------------------------------------------------------

path_datos <- "Datos"
path_mst <- "Resultados_Script2/MST_normalizados"
path_centroides <- "Resultados_Script1"

path_resultados <- "Resultados_Script6"
path_figuras <- file.path(
  path_resultados,
  "Figuras_probabilidad"
)

if (!dir.exists(path_resultados)) {
  dir.create(path_resultados)
}

if (!dir.exists(path_figuras)) {
  dir.create(path_figuras)
}


# ---------------------------------------------------------
# 2. Colores de los intervalos de probabilidad
# ---------------------------------------------------------

colores <- c(
  "0-20" = "#FFFAAC",
  "20-40" = "#E0D28E",
  "40-60" = "#AFA995",
  "60-80" = "#3F6FA8",
  "80-100" = "#001C3F"
)


# ---------------------------------------------------------
# 3. Localizar los MST normalizados
# ---------------------------------------------------------

archivos_mst <- list.files(
  path_mst,
  pattern = "^mst_normalizado_.*\\.csv$",
  full.names = TRUE
)

if (length(archivos_mst) == 0) {
  stop("No se encontraron archivos MST normalizados.")
}


# ---------------------------------------------------------
# 4. Tabla global de resultados
# ---------------------------------------------------------

mst_probabilidad <- data.frame()


# ---------------------------------------------------------
# 5. Procesar cada región
# ---------------------------------------------------------

for (archivo in archivos_mst) {
  
  # Nombre de la región
  
  region <- sub(
    "^mst_normalizado_(.*)\\.csv$",
    "\\1",
    basename(archivo)
  )
  
  cat("Procesando:", region, "\n")
  
  
  # -------------------------------------------------------
  # Leer MST normalizado
  # -------------------------------------------------------
  
  mst <- read.csv(
    archivo,
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # Calcular probabilidad de conservación
  # -------------------------------------------------------
  
  mst$prob_conservacion <- ifelse(
    mst$dn <= 0.5,
    1 - 2 * mst$dn,
    0
  )
  
  mst$prob_conservacion_pct <-
    100 * mst$prob_conservacion
  
  
  # -------------------------------------------------------
  # Clasificar la probabilidad en intervalos
  # -------------------------------------------------------
  
  mst$clase_probabilidad <- cut(
    mst$prob_conservacion_pct,
    breaks = c(
      0,
      20,
      40,
      60,
      80,
      100
    ),
    include.lowest = TRUE,
    labels = c(
      "0-20",
      "20-40",
      "40-60",
      "60-80",
      "80-100"
    )
  )
  
  mst$color <- colores[
    as.character(
      mst$clase_probabilidad
    )
  ]
  
  
  # Añadir región a la tabla global
  
  mst_probabilidad <- rbind(
    mst_probabilidad,
    mst
  )
  
  
  # -------------------------------------------------------
  # Leer estrellas y centroides
  # -------------------------------------------------------
  
  archivo_stars <- file.path(
    path_datos,
    paste0(
      region,
      ".csv"
    )
  )
  
  archivo_centroides <- file.path(
    path_centroides,
    paste0(
      "centroides_",
      region,
      ".csv"
    )
  )
  
  
  if (!file.exists(archivo_stars) ||
      !file.exists(archivo_centroides)) {
    
    warning(
      "Faltan datos gráficos para la región: ",
      region
    )
    
    next
  }
  
  
  stars <- read.csv(
    archivo_stars,
    stringsAsFactors = FALSE
  )
  
  centroides <- read.csv(
    archivo_centroides,
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # Localizar los extremos de las ramas
  # -------------------------------------------------------
  
  i1 <- match(
    mst$v1,
    centroides$iNest
  )
  
  i2 <- match(
    mst$v2,
    centroides$iNest
  )
  
  
  # -------------------------------------------------------
  # Representación gráfica
  # -------------------------------------------------------
  
  pdf(
    file.path(
      path_figuras,
      paste0(
        "probabilidad_",
        region,
        ".pdf"
      )
    ),
    width = 8,
    height = 8
  )
  
  
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
      "- Probabilidad de conservación"
    )
  )
  
  
  for (i in seq_len(nrow(mst))) {
    
    segments(
      centroides$x[i1[i]],
      centroides$y[i1[i]],
      centroides$x[i2[i]],
      centroides$y[i2[i]],
      col = mst$color[i],
      lwd = 2
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
    title = "Probabilidad"
  )
  
  
  dev.off()
}


# ---------------------------------------------------------
# 6. Guardar tabla global
# ---------------------------------------------------------

write.csv(
  mst_probabilidad,
  file.path(
    path_resultados,
    "mst_probabilidad_ramas.csv"
  ),
  row.names = FALSE
)


cat("Script6 finalizado.\n")