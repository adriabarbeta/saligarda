# ==============================================================================
# 10a_baixa_ncep.R -- Descarrega i guarda la reanalisi NCEP/NCAR R1
#   Font: NOAA PSL (2,5 graus, 6-horaria, 1948 enca), sense clau d'API.
#   El paquet synoptReg, que fa la classificacio sinoptica, va ser retirat del
#   CRAN l'octubre de 2023; aqui la classificacio es fa amb prcomp + kmeans,
#   que es el mateix metode (PCA en mode S i agrupament) sense la dependencia.
#   Sortida: derived/ncep_slp.rds i derived/ncep_z500.rds (es reutilitzen)
# ==============================================================================
suppressMessages({library(RNCEP)})

if (!dir.exists("derived")) dir.create("derived")

# finestra: Atlantic proper, Peninsula, golf de Lleo i Mediterrania occidental
LAT <- c(32.5, 52.5); LON <- c(-20, 20)
ANY_INI <- 1980; ANY_FI <- 2024

baixa <- function(variable, level, fitxer, a_ini, a_fi) {
  if (file.exists(fitxer)) {
    cat("ja existeix:", fitxer, "\n"); return(invisible(readRDS(fitxer)))
  }
  trossos <- list()
  for (a in a_ini:a_fi) {
    cat(sprintf("  %s %d ... ", variable, a)); flush.console()
    x <- try(NCEP.gather(variable = variable, level = level,
                         months.minmax = c(1, 12), years.minmax = c(a, a),
                         lat.southnorth = LAT, lon.westeast = LON,
                         reanalysis2 = FALSE, return.units = FALSE,
                         status.bar = FALSE), silent = TRUE)
    if (inherits(x, "try-error")) { cat("FALLA\n"); next }
    trossos[[as.character(a)]] <- x
    cat(dim(x)[3], "passos\n")
  }
  if (!length(trossos)) {
    cat("cap any descarregat per a", variable, "\n"); return(invisible(NULL))
  }
  res <- list(dades = trossos,
              lat = as.numeric(dimnames(trossos[[1]])[[1]]),
              lon = as.numeric(dimnames(trossos[[1]])[[2]]))
  saveRDS(res, fitxer)
  cat("-> ", fitxer, "\n")
  invisible(res)
}

cat("=== Pressio al nivell del mar, 1980-2024 ===\n")
baixa("slp", "surface", "derived/ncep_slp.rds", ANY_INI, ANY_FI)

cat("\n=== Geopotencial a 500 hPa, 2011-2024 ===\n")
baixa("hgt", 500, "derived/ncep_z500.rds", 2011, ANY_FI)   # level numeric, no text

cat("\nFet.\n")
