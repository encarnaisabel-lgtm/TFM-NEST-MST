# =========================================================
# Script3_poda_por_metodo.R
# Aplicación de un método de poda al MST normalizado
# =========================================================

rm(list = ls())

# ---------------------------------------------------------
# 1. Rutas
# ---------------------------------------------------------

path_datos <- "Datos"
path_mst <- "Resultados_Script2/MST_normalizados"
path_centroides <- "Resultados_Script1"


# ---------------------------------------------------------
# 2. Método y parámetros de poda
# ---------------------------------------------------------

# Escribe el método que quieres aplicar:
# "iqr"
# "p90"
# "p95"
# "media_sd"
# "dn_fijo"
# "alfa_dn_max"

criterio_poda <- "dn_fijo"


# Escribe el valor de dn fijo
dn_fijo <- 0.25

# Escribe el valor de k para el criterio IQR
k_iqr <- 1.5

# Escribe el valor de alfa
alfa <- 0.5


# ---------------------------------------------------------
# 3. Carpeta de resultados
# ---------------------------------------------------------

path_resultados <- paste0(
  "Resultados_Script3_poda_",
  criterio_poda
)

path_figuras <- file.path(
  path_resultados,
  "Figuras_poda"
)

if (!dir.exists(path_resultados)) {
  dir.create(path_resultados)
}

if (!dir.exists(path_figuras)) {
  dir.create(path_figuras)
}


# ---------------------------------------------------------
# 4. Función para calcular el umbral de poda
# ---------------------------------------------------------

calcularUmbralPoda <- function(dn, metodo) {
  
  dn <- dn[is.finite(dn)]
  
  if (metodo == "iqr") {
    
    q1 <- quantile(dn, 0.25)
    q3 <- quantile(dn, 0.75)
    
    iqr <- q3 - q1
    
    return(
      as.numeric(
        q3 + k_iqr * iqr
      )
    )
  }
  
  
  if (metodo == "p90") {
    
    return(
      as.numeric(
        quantile(dn, 0.90)
      )
    )
  }
  
  
  if (metodo == "p95") {
    
    return(
      as.numeric(
        quantile(dn, 0.95)
      )
    )
  }
  
  
  if (metodo == "media_sd") {
    
    return(
      mean(dn) + sd(dn)
    )
  }
  
  
  if (metodo == "dn_fijo") {
    
    return(dn_fijo)
  }
  
  
  if (metodo == "alfa_dn_max") {
    
    return(
      alfa * max(dn)
    )
  }
  
  
  stop("Método de poda no reconocido.")
}


# ---------------------------------------------------------
# 5. Localizar los MST normalizados
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
# 6. Ordenar las regiones
# ---------------------------------------------------------

nombre_archivo <- basename(archivos_mst)

realizacion <- as.integer(
  sub(
    ".*_([0-9]+)\\.csv$",
    "\\1",
    nombre_archivo
  )
)

distribucion <- sub(
  "_[0-9]+\\.csv$",
  "",
  nombre_archivo
)


archivos_mst <- archivos_mst[
  order(
    orden_distribucion,
    realizacion
  )
]


# ---------------------------------------------------------
# 7. Objetos para guardar resultados
# ---------------------------------------------------------

mst_podado_global <- data.frame()

resumen_poda <- data.frame()


# ---------------------------------------------------------
# 8. Procesar cada región
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
  # Leer MST
  # -------------------------------------------------------
  
  mst <- read.csv(
    archivo,
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # Calcular umbral de poda
  # -------------------------------------------------------
  
  umbral <- calcularUmbralPoda(
    mst$dn,
    criterio_poda
  )
  
  
  # -------------------------------------------------------
  # Podar MST
  # -------------------------------------------------------
  
  mst_podado <- mst[
    mst$dn <= umbral,
  ]
  
  
  # -------------------------------------------------------
  # Resumen de la región
  # -------------------------------------------------------
  
  resumen_region <- data.frame(
    
    region = region,
    
    tipo = mst$tipo[1],
    
    criterio = criterio_poda,
    
    umbral = umbral,
    
    n_aristas_original = nrow(mst),
    
    n_aristas_podado = nrow(mst_podado),
    
    n_aristas_eliminadas =
      nrow(mst) - nrow(mst_podado),
    
    fraccion_eliminada =
      (nrow(mst) - nrow(mst_podado)) / nrow(mst)
  )
  
  
  resumen_poda <- rbind(
    resumen_poda,
    resumen_region
  )
  
  
  # -------------------------------------------------------
  # Guardar MST podado en el conjunto global
  # -------------------------------------------------------
  
  if (nrow(mst_podado) > 0) {
    
    mst_podado$region <- region
    mst_podado$criterio <- criterio_poda
    mst_podado$umbral <- umbral
    
    mst_podado_global <- rbind(
      mst_podado_global,
      mst_podado
    )
  }
  
  
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
  
  i1_original <- match(
    mst$v1,
    centroides$iNest
  )
  
  i2_original <- match(
    mst$v2,
    centroides$iNest
  )
  
  
  i1_podado <- match(
    mst_podado$v1,
    centroides$iNest
  )
  
  i2_podado <- match(
    mst_podado$v2,
    centroides$iNest
  )
  
  
  # -------------------------------------------------------
  # Representación gráfica
  # -------------------------------------------------------
  
  pdf(
    file.path(
      path_figuras,
      paste0(
        "poda_MST_",
        region,
        ".pdf"
      )
    ),
    width = 10,
    height = 5
  )
  
  
  par(
    mfrow = c(1, 2),
    mar = c(4, 4, 3, 1)
  )
  
  
  # -------------------------------------------------------
  # MST original
  # -------------------------------------------------------
  
  plot(
    stars$ra,
    stars$dec,
    pch = 16,
    cex = 0.45,
    col = "grey80",
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
  
  
  for (i in seq_len(nrow(mst))) {
    
    segments(
      centroides$x[i1_original[i]],
      centroides$y[i1_original[i]],
      centroides$x[i2_original[i]],
      centroides$y[i2_original[i]],
      col = "grey30",
      lwd = 1
    )
  }
  
  
  points(
    centroides$x,
    centroides$y,
    pch = 19,
    cex = 0.8
  )
  
  
  # -------------------------------------------------------
  # MST podado
  # -------------------------------------------------------
  
  plot(
    stars$ra,
    stars$dec,
    pch = 16,
    cex = 0.45,
    col = "grey80",
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
      "- Podado"
    )
  )
  
  
  if (nrow(mst_podado) > 0) {
    
    for (i in seq_len(nrow(mst_podado))) {
      
      segments(
        centroides$x[i1_podado[i]],
        centroides$y[i1_podado[i]],
        centroides$x[i2_podado[i]],
        centroides$y[i2_podado[i]],
        col = "steelblue4",
        lwd = 1.5
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
  # Información del método aplicado
  # -------------------------------------------------------
  
  mtext(
    paste(
      "Umbral",
      criterio_poda,
      "=",
      round(
        umbral,
        3
      )
    ),
    side = 3,
    line = -1.5,
    cex = 0.8
  )
  
  
  mtext(
    paste(
      "Nº aristas original:",
      nrow(mst),
      "| Nº aristas eliminadas:",
      nrow(mst) - nrow(mst_podado)
    ),
    side = 3,
    line = -2.7,
    cex = 0.75
  )
  
  
  dev.off()
}


# ---------------------------------------------------------
# 9. Guardar resultados
# ---------------------------------------------------------

write.csv(
  mst_podado_global,
  file.path(
    path_resultados,
    "mst_normalizados_podado_global.csv"
  ),
  row.names = FALSE
)


write.csv(
  resumen_poda,
  file.path(
    path_resultados,
    "resumen_poda_por_region.csv"
  ),
  row.names = FALSE
)


cat(
  "Script3 finalizado. Método aplicado:",
  criterio_poda,
  "\n"
)