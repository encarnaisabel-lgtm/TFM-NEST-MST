# =========================================================
# Script4_Poda_por_codo.R
# Poda del MST mediante el criterio del máximo salto
# =========================================================

rm(list = ls())

library(ggplot2)
library(patchwork)


# ---------------------------------------------------------
# 1. Rutas
# ---------------------------------------------------------

path_datos <- "Datos"
path_mst <- "Resultados_Script2/MST_normalizados"
path_centroides <- "Resultados_Script1"

path_resultados <- "Resultados_Script3_poda_codo"

if (!dir.exists(path_resultados)) {
  dir.create(path_resultados)
}

pdf_salida <- file.path(
  path_resultados,
  "Poda_por_codo.pdf"
)


# ---------------------------------------------------------
# 2. Localizar los MST normalizados
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
# 3. Ordenar las regiones
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
  order(distribucion, realizacion)
]


# ---------------------------------------------------------
# 4. Tabla de resultados
# ---------------------------------------------------------

resumen_codos <- data.frame()


# ---------------------------------------------------------
# 5. Abrir PDF
# ---------------------------------------------------------

pdf(
  pdf_salida,
  width = 13,
  height = 7
)


# ---------------------------------------------------------
# 6. Procesar cada región
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
  # Leer estrellas y centroides
  # -------------------------------------------------------
  
  stars <- read.csv(
    file.path(
      path_datos,
      paste0(region, ".csv")
    ),
    stringsAsFactors = FALSE
  )
  
  centroides <- read.csv(
    file.path(
      path_centroides,
      paste0(
        "centroides_",
        region,
        ".csv"
      )
    ),
    stringsAsFactors = FALSE
  )
  
  
  # -------------------------------------------------------
  # Ordenar las ramas según dn
  # -------------------------------------------------------
  
  mst_ordenado <- mst[
    order(mst$dn),
  ]
  
  mst_ordenado$indice <- seq_len(
    nrow(mst_ordenado)
  )
  
  
  # -------------------------------------------------------
  # Calcular los saltos consecutivos
  # -------------------------------------------------------
  
  if (nrow(mst_ordenado) >= 2) {
    
    saltos <- diff(
      mst_ordenado$dn
    )
    
    i_max_salto <- which.max(
      saltos
    )
    
    max_salto <- saltos[
      i_max_salto
    ]
    
    # El corte se sitúa en la rama anterior al mayor salto
    dn_corte <- mst_ordenado$dn[
      i_max_salto
    ]
    
  } else {
    
    # Con una sola rama no puede calcularse un salto
    
    saltos <- numeric(0)
    
    i_max_salto <- NA
    max_salto <- NA
    dn_corte <- NA
  }
  
  
  # -------------------------------------------------------
  # Aplicar la poda
  # -------------------------------------------------------
  
  if (!is.na(dn_corte)) {
    
    mst_podado <- mst[
      mst$dn <= dn_corte,
    ]
    
  } else {
    
    mst_podado <- mst
  }
  
  
  # -------------------------------------------------------
  # Resultados de la región
  # -------------------------------------------------------
  
  n_aristas_eliminadas <-
    nrow(mst) - nrow(mst_podado)
  
  fraccion_eliminada <-
    n_aristas_eliminadas / nrow(mst)
  
  
  resumen_region <- data.frame(
    
    region = region,
    
    tipo = mst$tipo[1],
    
    criterio = "max_salto",
    
    dn_corte = dn_corte,
    
    n_aristas_original = nrow(mst),
    
    n_aristas_podado = nrow(mst_podado),
    
    n_aristas_eliminadas =
      n_aristas_eliminadas,
    
    fraccion_eliminada =
      fraccion_eliminada,
    
    i_max_salto =
      i_max_salto,
    
    max_salto =
      max_salto
  )
  
  
  resumen_codos <- rbind(
    resumen_codos,
    resumen_region
  )
  
  
  # -------------------------------------------------------
  # Coordenadas de las ramas
  # -------------------------------------------------------
  
  i1 <- match(
    mst$v1,
    centroides$iNest
  )
  
  i2 <- match(
    mst$v2,
    centroides$iNest
  )
  
  
  segmentos <- data.frame(
    
    x1 = centroides$x[i1],
    y1 = centroides$y[i1],
    
    x2 = centroides$x[i2],
    y2 = centroides$y[i2],
    
    dn = mst$dn
  )
  
  
  # Clasificar las ramas
  
  if (!is.na(dn_corte)) {
    
    segmentos$clase <- ifelse(
      segmentos$dn <= dn_corte,
      "Mantener",
      "Podar"
    )
    
  } else {
    
    segmentos$clase <- "Mantener"
  }
  
  
  # -------------------------------------------------------
  # GRÁFICO 1: MST espacial
  # -------------------------------------------------------
  
  p_mst <- ggplot() +
    
    geom_point(
      data = stars,
      aes(
        x = ra,
        y = dec
      ),
      size = 0.7,
      color = "grey70"
    ) +
    
    geom_segment(
      data = segmentos[
        segmentos$clase == "Mantener",
      ],
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      linewidth = 0.7
    ) +
    
    geom_segment(
      data = segmentos[
        segmentos$clase == "Podar",
      ],
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    
    geom_point(
      data = centroides,
      aes(
        x = x,
        y = y
      ),
      size = 2
    ) +
    
    scale_x_reverse() +
    
    coord_equal() +
    
    theme_minimal() +
    
    labs(
      title = "MST espacial",
      x = "RA",
      y = "DEC"
    )
  
  
  # -------------------------------------------------------
  # GRÁFICO 2: ramas ordenadas por dn
  # -------------------------------------------------------
  
  p1 <- ggplot(
    mst_ordenado,
    aes(
      x = indice,
      y = dn
    )
  ) +
    
    geom_line() +
    
    geom_point(
      size = 2
    )
  
  
  if (!is.na(i_max_salto)) {
    
    p1 <- p1 +
      
      geom_vline(
        xintercept = i_max_salto,
        linetype = "dashed"
      )
  }
  
  
  p1 <- p1 +
    
    theme_minimal() +
    
    labs(
      title = "Ramas ordenadas por dn",
      x = "Índice de rama ordenada",
      y = "dn"
    )
  
  
  # -------------------------------------------------------
  # GRÁFICO 3: saltos consecutivos
  # -------------------------------------------------------
  
  if (length(saltos) > 0) {
    
    tabla_saltos <- data.frame(
      
      indice = seq_along(saltos),
      
      salto = saltos
    )
    
    
    p2 <- ggplot(
      tabla_saltos,
      aes(
        x = indice,
        y = salto
      )
    ) +
      
      geom_col() +
      
      geom_vline(
        xintercept = i_max_salto,
        linetype = "dashed"
      ) +
      
      theme_minimal() +
      
      labs(
        title = "Saltos consecutivos",
        x = "Índice",
        y = expression(Delta * dn)
      )
    
    
  } else {
    
    p2 <- ggplot() +
      
      annotate(
        "text",
        x = 0,
        y = 0,
        label =
          "Solo existe una rama.\nNo se pueden calcular saltos.",
        size = 5
      ) +
      
      theme_void() +
      
      labs(
        title = "Saltos consecutivos"
      )
  }
  
  
  # -------------------------------------------------------
  # Título general
  # -------------------------------------------------------
  
  if (!is.na(dn_corte)) {
    
    titulo_general <- paste0(
      
      region,
      
      " | tipo = ",
      mst$tipo[1],
      
      " | aristas = ",
      nrow(mst),
      
      " | eliminadas = ",
      n_aristas_eliminadas,
      
      " | dn corte = ",
      round(
        dn_corte,
        4
      )
    )
    
  } else {
    
    titulo_general <- paste0(
      
      region,
      
      " | tipo = ",
      mst$tipo[1],
      
      " | aristas = ",
      nrow(mst),
      
      " | eliminadas = 0",
      
      " | sin corte"
    )
  }
  
  
  # -------------------------------------------------------
  # Montar página
  # -------------------------------------------------------
  
  pagina <- (
    
    p_mst | p1 | p2
    
  ) +
    
    plot_annotation(
      title = titulo_general
    )
  
  
  print(
    pagina
  )
}


# ---------------------------------------------------------
# 7. Cerrar PDF
# ---------------------------------------------------------

dev.off()


# ---------------------------------------------------------
# 8. Guardar resultados
# ---------------------------------------------------------

write.csv(
  resumen_codos,
  file.path(
    path_resultados,
    "resumen_poda_por_region.csv"
  ),
  row.names = FALSE
)


cat("Script4 finalizado.\n")