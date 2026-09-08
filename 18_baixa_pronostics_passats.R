# ==============================================================================
# 18_baixa_pronostics_passats.R -- Arxiu de PREDICCIONS passades d'Open-Meteo
#   L'historical-forecast-api guarda el que els models numerics van predir de
#   debo, no l'analisi posterior. Amb aixo es pot mesurar la destresa REAL del
#   bot, sense l'optimisme de validar amb ERA5.
#   Sortida: derived/openmeteo_pronostics_passats.rds
# ==============================================================================
suppressMessages({library(jsonlite); library(data.table)})
PUNTS <- data.table(nom = c("garriga","nord","sud","oest"),
                    lat = c(41.6833, 45.00, 40.50, 41.65),
                    lon = c( 2.2833,  2.00,  3.00, -0.90))
VARS <- c("temperature_2m","relative_humidity_2m","dew_point_2m","pressure_msl",
          "cloud_cover","cloud_cover_low","cloud_cover_mid","cloud_cover_high",
          "wind_speed_10m","wind_direction_10m","shortwave_radiation")
FITXER <- "derived/openmeteo_pronostics_passats.rds"
if (file.exists(FITXER)) { cat("ja existeix\n"); quit(save="no") }

baixa <- function(lat, lon, d1, d2) {
  u <- sprintf(paste0("https://historical-forecast-api.open-meteo.com/v1/forecast",
                      "?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s",
                      "&hourly=%s&timezone=UTC"), lat, lon, d1, d2, paste(VARS, collapse=","))
  for (i in 1:6) {
    r <- try(suppressWarnings(fromJSON(u)), silent=TRUE)
    if (!inherits(r,"try-error") && !is.null(r$hourly)) return(as.data.table(r$hourly))
    Sys.sleep(15*i)
  }
  NULL
}
res <- list()
for (i in seq_len(nrow(PUNTS))) {
  tr <- list()
  for (a in 2022:2024) {
    cat(sprintf("  %-8s %d ... ", PUNTS$nom[i], a)); flush.console()
    x <- baixa(PUNTS$lat[i], PUNTS$lon[i], sprintf("%d-01-01", a), sprintf("%d-12-31", a))
    if (is.null(x)) { cat("FALLA\n"); next }
    cat(nrow(x), "hores\n"); tr[[as.character(a)]] <- x
  }
  if (!length(tr)) next
  d <- rbindlist(tr, fill=TRUE)
  d[, datetime := as.POSIXct(time, format="%Y-%m-%dT%H:%M", tz="UTC")][, time := NULL]
  setnames(d, setdiff(names(d),"datetime"), paste0(PUNTS$nom[i],"_",setdiff(names(d),"datetime")))
  res[[PUNTS$nom[i]]] <- unique(d, by="datetime")
}
OM <- Reduce(function(a,b) merge(a,b,by="datetime",all=TRUE), res)
setorder(OM, datetime)
saveRDS(OM, FITXER)
cat(sprintf("\n-> %s: %d hores\n", FITXER, nrow(OM)))
