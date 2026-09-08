# ==============================================================================
# 14_baixa_openmeteo.R -- Arxiu ERA5 d'Open-Meteo per entrenar el model operatiu
#
#   Per que aixo i no els PC de la reanalisi NCEP: el model operatiu ha de rebre
#   EXACTAMENT les mateixes variables que ha vist entrenant. Open-Meteo serveix
#   l'arxiu ERA5 i el pronostic amb els mateixos noms i les mateixes unitats, de
#   manera que el desajust entre entrenament i operacio es minim. A mes hi ha la
#   NUVOLOSITAT, que es el predictor que faltava: la intensitat del drenatge la
#   mana el refredament radiatiu de la nit.
#
#   Quatre punts: la Garriga i tres mes per calcular gradients de pressio
#   regionals, que fan la feina de les components principals sense PCA.
#
#   Sortida: derived/openmeteo_arxiu.rds (es cacheja; no cal tornar-ho a baixar)
# ==============================================================================
suppressMessages({library(jsonlite); library(data.table)})

PUNTS <- data.table(
  nom = c("garriga", "nord", "sud", "oest"),
  lat = c(41.6833, 45.00, 40.50, 41.65),
  lon = c( 2.2833,  2.00,  3.00, -0.90))

VARS <- c("temperature_2m", "relative_humidity_2m", "dew_point_2m", "pressure_msl",
          "cloud_cover", "cloud_cover_low", "cloud_cover_mid", "cloud_cover_high",
          "wind_speed_10m", "wind_direction_10m", "shortwave_radiation")

ANY_INI <- 2012; ANY_FI <- 2024
FITXER  <- "derived/openmeteo_arxiu.rds"

if (file.exists(FITXER)) {
  cat("ja existeix:", FITXER, " (esborra'l per tornar-lo a baixar)\n")
} else {
  if (!dir.exists("derived")) dir.create("derived")
  baixa_tros <- function(lat, lon, d1, d2) {
    u <- sprintf(paste0("https://archive-api.open-meteo.com/v1/archive",
                        "?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s",
                        "&hourly=%s&timezone=UTC"),
                 lat, lon, d1, d2, paste(VARS, collapse = ","))
    for (intent in 1:4) {
      r <- try(fromJSON(u), silent = TRUE)
      if (!inherits(r, "try-error") && !is.null(r$hourly)) return(as.data.table(r$hourly))
      Sys.sleep(5 * intent)
    }
    NULL
  }
  trams <- data.table(d1 = sprintf("%d-01-01", seq(ANY_INI, ANY_FI, by = 3)))
  trams[, d2 := sprintf("%d-12-31", pmin(as.integer(substr(d1, 1, 4)) + 2, ANY_FI))]

  res <- list()
  for (i in seq_len(nrow(PUNTS))) {
    trossos <- list()
    for (k in seq_len(nrow(trams))) {
      cat(sprintf("  %-8s %s..%s ... ", PUNTS$nom[i], trams$d1[k], trams$d2[k]))
      flush.console()
      x <- baixa_tros(PUNTS$lat[i], PUNTS$lon[i], trams$d1[k], trams$d2[k])
      if (is.null(x)) { cat("FALLA\n"); next }
      cat(nrow(x), "hores\n"); trossos[[k]] <- x
    }
    d <- rbindlist(trossos, fill = TRUE)
    d[, datetime := as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "UTC")]
    d[, time := NULL]
    setnames(d, setdiff(names(d), "datetime"),
             paste0(PUNTS$nom[i], "_", setdiff(names(d), "datetime")))
    res[[PUNTS$nom[i]]] <- unique(d, by = "datetime")
  }
  OM <- Reduce(function(a, b) merge(a, b, by = "datetime", all = TRUE), res)
  setorder(OM, datetime)
  saveRDS(OM, FITXER)
  cat(sprintf("\n-> %s: %d hores, %d columnes\n", FITXER, nrow(OM), ncol(OM)))
  cat("   periode:", format(min(OM$datetime)), "->", format(max(OM$datetime)), "\n")
}
