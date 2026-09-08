# ==============================================================================
# 02_build_garriga.R -- Reconstrueix la serie de 5 min de LA GARRIGA (Davis)
#   Llegeix els ~4.700 Excel diaris de Saligarda/Dades/Dades/<any>/<mes>/
#   Gestiona capcaleres en angles (Date/Time) i en catala (Data/Hora),
#   nombre de columnes variable i files de peu de full.
#   Sortida: derived/la_garriga_5min.csv
#   Us: Rscript 02_build_garriga.R      (N_TEST=24 Rscript ... per provar)
# ==============================================================================
suppressMessages({library(readxl); library(data.table); library(future.apply)})

base_dir <- "Saligarda/Dades/Dades"
n_test   <- as.integer(Sys.getenv("N_TEST", "0"))   # >0 = mode prova

# --- normalitzacio de noms de columna ---------------------------------------
norm <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(ifelse(is.na(x), "", x))
  gsub("[^a-z0-9]", "", x)
}
DIC <- c(
  date = "date", data = "date",
  time = "time", hora = "time",
  tempout = "temp_out", tempext = "temp_out", ext = "temp_out",
  hitemp = "hi_temp", tempmax = "hi_temp",
  lowtemp = "low_temp", tempmin = "low_temp",
  outhum = "out_hum", humext = "out_hum",
  windrun = "wind_run", recvent = "wind_run",
  hispeed = "hi_speed", velmax = "hi_speed",
  hidir = "hi_dir", dirmax = "hi_dir",
  windchill = "wind_chill", sensterm = "wind_chill",
  lowchill = "low_chill", senstmin = "low_chill",
  bar = "bar", mb = "bar", pressio = "bar",
  rain = "rain", pluja = "rain",
  rainrate = "rain_rate", intpluja = "rain_rate"
)
CANON <- c("date","time","temp_out","hi_temp","low_temp","out_hum","wind_run",
           "hi_speed","hi_dir","wind_chill","low_chill","bar","rain","rain_rate")

# --- lector d'un fitxer diari ------------------------------------------------
llegeix_dia <- function(f) {
  h <- suppressMessages(read_excel(f, n_max = 2, col_names = FALSE,
                                   col_types = "text", .name_repair = "minimal"))
  hdr <- apply(h, 2, function(x) paste(trimws(ifelse(is.na(x), "", x)), collapse = " "))
  key <- DIC[norm(hdr)]

  d <- suppressMessages(read_excel(f, skip = 2, col_names = FALSE,
                                   col_types = "text", .name_repair = "minimal"))
  if (nrow(d) == 0) return(NULL)
  d <- as.data.table(d)
  keep <- which(!is.na(key) & !duplicated(key) & seq_along(key) <= ncol(d))
  if (!length(keep)) return(NULL)
  d <- d[, keep, with = FALSE]
  setnames(d, unname(key[keep]))
  for (cn in setdiff(CANON, names(d))) d[, (cn) := NA_character_]
  d <- d[, CANON, with = FALSE]

  # data: numero de serie d'Excel o text dd/mm/aaaa
  dnum <- suppressWarnings(as.numeric(d$date))
  dia  <- as.Date(dnum, origin = "1899-12-30")
  txt  <- is.na(dia) & !is.na(d$date)
  if (any(txt)) {
    dia[txt] <- as.Date(d$date[txt], optional = TRUE,
                        tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y"))
  }
  # hora: fraccio de dia o text HH:MM(:SS)
  hnum <- suppressWarnings(as.numeric(d$time))
  segs <- round(hnum * 86400 / 60) * 60
  txt  <- is.na(segs) & !is.na(d$time)
  if (any(txt)) {
    # files de peu de full (MITJANES, MAX-MIN, PROMIG...) cauen a NA i es descarten
    pp <- strsplit(d$time[txt], ":", fixed = TRUE)
    tros <- function(z, i) if (length(z) >= i) z[i] else NA_character_
    hh <- suppressWarnings(as.numeric(vapply(pp, tros, "", 1L)))
    mm <- suppressWarnings(as.numeric(vapply(pp, tros, "", 2L)))
    segs[txt] <- hh * 3600 + mm * 60
  }
  d[, datetime := as.POSIXct(dia, tz = "UTC") + segs]
  d <- d[!is.na(datetime)]
  if (!nrow(d)) return(NULL)

  for (cn in c("temp_out","hi_temp","low_temp","out_hum","wind_run","hi_speed",
               "wind_chill","low_chill","bar","rain","rain_rate")) {
    d[, (cn) := suppressWarnings(as.numeric(get(cn)))]
  }
  d[, hi_dir := toupper(trimws(hi_dir))]
  d[, `:=`(date = NULL, time = NULL, file = basename(f))]
  d[]
}

# --- llista de fitxers -------------------------------------------------------
fitxers <- list.files(base_dir, pattern = "[.]xlsx?$", recursive = TRUE,
                      full.names = TRUE)
fitxers <- fitxers[!startsWith(basename(fitxers), "~$")]
if (n_test > 0) fitxers <- fitxers[round(seq(1, length(fitxers), length.out = n_test))]
cat("Fitxers a llegir:", length(fitxers), "\n")

plan(multisession, workers = max(1, min(6, parallel::detectCores() - 1)))
res <- future_lapply(fitxers, function(f) {
  tryCatch(llegeix_dia(f),
           error = function(e) data.table(file = basename(f), err = conditionMessage(e)))
})
plan(sequential)

es_error <- vapply(res, function(x) !is.null(x) && "err" %in% names(x), logical(1))
cat("Fitxers amb error:", sum(es_error), "\n")
if (any(es_error)) print(head(rbindlist(res[es_error]), 10))

ok <- vapply(res, function(x) !is.null(x) && "datetime" %in% names(x), logical(1))
gar <- rbindlist(res[ok], fill = TRUE)
setorder(gar, datetime)
cat("Registres bruts:", nrow(gar), "\n")

# --- control de qualitat -----------------------------------------------------
gar <- unique(gar, by = "datetime")
gar[temp_out < -25 | temp_out >   48, temp_out   := NA]
gar[hi_temp  < -25 | hi_temp  >   48, hi_temp    := NA]
gar[low_temp < -25 | low_temp >   48, low_temp   := NA]
gar[out_hum  <   0 | out_hum  >  100, out_hum    := NA]
gar[hi_speed <   0 | hi_speed >  160, hi_speed   := NA]
gar[bar      < 940 | bar      > 1060, bar        := NA]
gar[rain     <   0 | rain     >  100, rain       := NA]
gar[wind_run <   0 | wind_run >   20, wind_run   := NA]

# direccio -> graus (nomenclatura anglesa i catalana/castellana)
GRAUS <- c(N = 0, NNE = 22.5, NE = 45, ENE = 67.5, E = 90, ESE = 112.5,
           SE = 135, SSE = 157.5, S = 180, SSW = 202.5, SSO = 202.5,
           SW = 225, SO = 225, WSW = 247.5, OSO = 247.5, W = 270, O = 270,
           WNW = 292.5, ONO = 292.5, NW = 315, NO = 315,
           NNW = 337.5, NNO = 337.5)
gar[, hi_dir_deg := unname(GRAUS[hi_dir])]
gar[is.na(hi_dir_deg), hi_dir := NA_character_]

# velocitat mitjana de l'interval a partir del recorregut de vent (km per pas)
gar[, dt_min := as.numeric(difftime(datetime, shift(datetime), units = "mins"))]
gar[, wind_mean := fifelse(!is.na(wind_run) & dt_min > 0 & dt_min <= 15,
                           wind_run / (dt_min / 60), NA_real_)]
gar[wind_mean > 160, wind_mean := NA]

if (!dir.exists("derived")) dir.create("derived")
fwrite(gar, "derived/la_garriga_5min.csv")

cat("\n-> derived/la_garriga_5min.csv:", nrow(gar), "registres\n")
cat("   periode:", format(min(gar$datetime)), "->", format(max(gar$datetime)), "\n")
gar[, any := year(datetime)]
print(gar[, .(n = .N,
              temp = sum(!is.na(temp_out)), hum = sum(!is.na(out_hum)),
              vel  = sum(!is.na(hi_speed)), dir = sum(!is.na(hi_dir_deg)),
              vmit = sum(!is.na(wind_mean)), bar = sum(!is.na(bar))),
          by = any][order(any)])
