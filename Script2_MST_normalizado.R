# =========================================================
# Script2_MST_normalizado.R
# Continuación de Script1
# =========================================================
# Objetivo:
#   1) Calcular la envolvente convexa de cada región.
#   2) Obtener el radio equivalente:
#        r = sqrt(A/pi)
#   3) Normalizar las ramas del MST:
#        dn = d/r
#   4) Guardar los MST normalizados.
#   5) Crear un archivo global con todos los resultados.
# =========================================================

rm(list = ls())

# ---------------------------------------------------------
# 1. Rutas
# ---------------------------------------------------------

path_datos <- "Datos"
path_salidas_script1 <- "Resultados_Script1"
path_resultados <- "Resultados_Script2"

if (!dir.exists(path_resultados)) {
  dir.create(path_resultados, recursive = TRUE)
}

path_figuras <- file.path(path_resultados, "Figuras_envolvente")
if (!dir.exists(path_figuras)) {
  dir.create(path_figuras, recursive = TRUE)
}

path_mst_normalizados <- file.path(path_resultados, "MST_normalizados")
if (!dir.exists(path_mst_normalizados)) {
  dir.create(path_mst_normalizados, recursive = TRUE)
}


# ---------------------------------------------------------
# 2. Función para calcular el área de un polígono
# ---------------------------------------------------------

areaPoligono <- function(x, y) {
  
  x2 <- c(x, x[1])
  y2 <- c(y, y[1])
  
  area <- 0.5 * abs(
    sum(
      x2[-1] * y2[-length(y2)] -
        x2[-length(x2)] * y2[-1]
    )
  )
  
  return(area)
}


# ---------------------------------------------------------
# 3. Archivos de regiones
# ---------------------------------------------------------

star_files <- list.files(
  path_datos,
  pattern = "\\.csv$",
  full.names = TRUE
)

regiones <- tools::file_path_sans_ext(
  basename(star_files)
)


# ---------------------------------------------------------
# 4. Objetos para guardar resultados globales
# ---------------------------------------------------------

resumen_regiones <- data.frame()

mst_normalizados <- data.frame()


# ---------------------------------------------------------
# 5. Procesar cada región
# ---------------------------------------------------------

for (i in seq_along(regiones)) {
  
  region_name <- regiones[i]
  stars_file <- star_files[i]
  
  cat("Procesando:", region_name, "\n")
  
  # Leer estrellas
  stars <- read.csv(
    stars_file,
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # Envolvente convexa
  # -------------------------------------------------------
  
  h <- chull(stars$ra, stars$dec)
  
  hull_xy <- stars[h, c("ra", "dec")]
  
  area <- areaPoligono(
    hull_xy$ra,
    hull_xy$dec
  )
  
  radio <- sqrt(area / pi)
  
  
  # Cerrar el polígono para dibujarlo
  hull_plot <- rbind(
    hull_xy,
    hull_xy[1, ]
  )
  
  
  # -------------------------------------------------------
  # Leer MST generado por Script1
  # -------------------------------------------------------
  
  archivo_mst <- file.path(
    path_salidas_script1,
    paste0("mst_", region_name, ".csv")
  )
  
  mst <- NULL
  
  if (file.exists(archivo_mst)) {
    
    mst_temp <- read.csv(
      archivo_mst,
      stringsAsFactors = FALSE
    )
    
    if (all(c("v1", "v2", "d") %in% names(mst_temp))) {
      mst <- mst_temp
    }
  }
  
  
  # -------------------------------------------------------
  # Tipo de distribución
  # -------------------------------------------------------
  
  tipo_region <- sub(
    "_[^_]+$",
    "",
    region_name
  )
  
  
  # -------------------------------------------------------
  # Normalizar MST
  # -------------------------------------------------------
  
  if (!is.null(mst) && nrow(mst) > 0) {
    
    mst$region <- region_name
    mst$radio_eq <- radio
    mst$dn <- mst$d / radio
    mst$tipo <- tipo_region
    
    
    # Guardar MST normalizado de la región
    
    write.csv(
      mst,
      file.path(
        path_mst_normalizados,
        paste0(
          "mst_normalizado_",
          region_name,
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    
    # Añadir al archivo global
    
    mst_normalizados <- rbind(
      mst_normalizados,
      mst
    )
    
    n_aristas <- nrow(mst)
    
  } else {
    
    n_aristas <- 0
  }
  
  
  # -------------------------------------------------------
  # Resumen de la región
  # -------------------------------------------------------
  
  resumen_i <- data.frame(
    region = region_name,
    area_hull = area,
    radio_eq = radio,
    n_estrellas = nrow(stars),
    n_aristas_mst = n_aristas,
    tipo = tipo_region
  )
  
  resumen_regiones <- rbind(
    resumen_regiones,
    resumen_i
  )
  
  
  # -------------------------------------------------------
  # Figura de la envolvente convexa
  # -------------------------------------------------------
  
  pdf(
    file.path(
      path_figuras,
      paste0(
        "envolvente_",
        region_name,
        ".pdf"
      )
    )
  )
  
  plot(
    stars$ra,
    stars$dec,
    pch = 16,
    cex = 0.6,
    asp = 1,
    xlab = "RA",
    ylab = "DEC",
    main = paste(
      "Región",
      region_name,
      "- Envolvente convexa"
    )
  )
  
  lines(
    hull_plot$ra,
    hull_plot$dec,
    lwd = 2
  )
  
  dev.off()
}


# ---------------------------------------------------------
# 6. Guardar resultados globales
# ---------------------------------------------------------

write.csv(
  resumen_regiones,
  file.path(
    path_resultados,
    "resumen_regiones.csv"
  ),
  row.names = FALSE
)

write.csv(
  mst_normalizados,
  file.path(
    path_resultados,
    "mst_normalizados_global.csv"
  ),
  row.names = FALSE
)


cat("Script2 finalizado.\n")