source('funcionesAuxiliares.R') # cargamos datos y funciones

# 0) CReo un directorio de salida.
dir_salida <- "Resultados_Script1"
if (!dir.exists(dir_salida)) dir.create(dir_salida)

# 1) Buscar todos los .csv en la carpeta Datos
star_files <- list.files("Datos", pattern="\\.csv$", full.names=TRUE)

# 2) Obtener nombre base (sin carpeta ni extensión)
base <- tools::file_path_sans_ext(basename(star_files))

# 3) Construir automáticamente el nombre del .RData asociado
rdata_files <- file.path("Datos", paste0(base, ".csv.Rdata"))

# 4) Crear tabla final
lista <- data.frame(
  base = base,
  stars_file = star_files,
  structs_file = rdata_files,
  structs_ok = file.exists(rdata_files),
  stringsAsFactors = FALSE
)

# 5) Ver resultado
print(head(lista))
message(sum(lista$structs_ok), " regiones con estructuras encontradas")

# bucle automatico
for (i in 1:nrow(lista)) {
  
  if (!lista$structs_ok[i]) next  # saltar si no existe el .RData
  
  region_name <- lista$base[i]
  stars_file  <- lista$stars_file[i]
  structs_file <- lista$structs_file[i]
  
  cat("Procesando:", region_name, "\n")
  
  # 1) Leer estrellas
  stars <- read.csv(stars_file, header = TRUE)
  
  # 2) Cargar estructuras
  ok_load <- tryCatch({
    load(structs_file)
    TRUE
  }, error = function(e) {
    message("Archivo corrupto o no legible: ", structs_file)
    FALSE
  })
  
  if (!ok_load) next   # debe cargar objeto structs2
  
  # 3) Calcular MST si procede
  if (!is.null(structs2$Nsub) && structs2$Nsub >= 2) {
    distNESTs <- distClust(structs2$Distsphe, structs2$Sub_Part)
    mstNESTs  <- MstDist(distNESTs)
  } else {
    mstNESTs <- NULL
  } 
  
  #añadimos salidas centroides_<region>.csv  mst_<region>.csv MST_resumen_<region>.pdf (plot+histograna)
  # --- 4a) Centroides (y dibujar NESTs) ---
  centrosNEST <- data.frame(iNest=integer(), x=double(), y=double())
  
  Nsub=max(stars$NEST)
  if (!is.null(structs2$Nsub) && structs2$Nsub >= 1) {
    for (iNest in 1:structs2$Nsub) {
      inNEST <- (structs2$Sub_Part == iNest)
      if (sum(inNEST) == 0) next
      x <- mean(stars$ra[inNEST])
      y <- mean(stars$dec[inNEST])
      centrosNEST <- rbind(centrosNEST, data.frame(iNest=iNest, x=x, y=y))
    }
  }
  
  # Añadir nombre de región y guardar CSV centroides
  if (nrow(centrosNEST) > 0) {
    centrosNEST$region <- region_name
  } else {
    centrosNEST <- data.frame(iNest=integer(), x=double(), y=double(), region=character())
  }
  
  write.csv(centrosNEST,
            file.path(dir_salida, paste0("centroides_", region_name, ".csv")),
            row.names = FALSE)
  
  # --- 4b) Guardar MST en CSV ---
  if (!is.null(mstNESTs) && nrow(mstNESTs) > 0) {
    mst_out <- mstNESTs
    mst_out$region <- region_name
    write.csv(mst_out,
              file.path(dir_salida, paste0("mst_", region_name, ".csv")),
              row.names = FALSE)
  } else {
    write.csv(data.frame(region = region_name,
                         Nsub = structs2$Nsub,
                         mensaje = "No hay MST (Nsub < 2)"),
              file.path(dir_salida, paste0("mst_", region_name, ".csv")),
              row.names = FALSE)
  }
  
  # --- 4c) PDF con (página 1) plot espacial y (página 2) histograma ---
  pdf(file.path(dir_salida, paste0("MST_resumen_", region_name, ".pdf")), width=12, height=6)
  par(mfrow = c(1, 2), mar = c(4,4,2,1))
  
  # Página 1: mapa
  plot(stars$ra, stars$dec, pch=19, col="grey",
       xlim = rev(range(stars$ra, na.rm = TRUE)),
       main = paste("Distribución espacial -", region_name, "\nNsub =", structs2$Nsub))
  
  # NESTs (cruces)
  if (!is.null(structs2$Nsub) && structs2$Nsub >= 1) {
    
    colores <- hcl.colors(structs2$Nsub, "Dark 3")
    
    for (iNest in 1:structs2$Nsub) {
      inNEST <- (structs2$Sub_Part == iNest)
      if (sum(inNEST) == 0) next
      points(stars$ra[inNEST], stars$dec[inNEST],
             pch = 3, col = colores[iNest])
    }
  }
  
  # centroides (azul)
  if (nrow(centrosNEST) > 0) {
    points(centrosNEST$x, centrosNEST$y, pch=19, col="blue")
  }
  
  # MST (líneas) usando match por si faltan iNest intermedios
  if (!is.null(mstNESTs) && nrow(mstNESTs) > 0 && nrow(centrosNEST) > 0) {
    for (k in 1:nrow(mstNESTs)) {
      p1 <- match(mstNESTs$v1[k], centrosNEST$iNest)
      p2 <- match(mstNESTs$v2[k], centrosNEST$iNest)
      if (is.na(p1) || is.na(p2)) next
      lines(c(centrosNEST$x[p1], centrosNEST$x[p2]),
            c(centrosNEST$y[p1], centrosNEST$y[p2]))
    }
  }
 
  
  # Página 2: histograma o mensaje
  if (!is.null(mstNESTs) && nrow(mstNESTs) > 0) {
    hist(mstNESTs$d,
         breaks = max(5, min(15, length(mstNESTs$d))),
         main = paste("Histograma aristas MST -", region_name, "\nNsub =", structs2$Nsub),
         xlab = "Longitud de arista",
         ylab = "Frecuencia")
  } else {
    plot.new()
    text(0.5, 0.6, paste("Región:", region_name), cex=1.3)
    text(0.5, 0.5, paste("Nsub =", structs2$Nsub), cex=1.2)
    text(0.5, 0.4, "No hay MST (Nsub < 2)", cex=1.2)
  }
  
  dev.off() 
  
}

