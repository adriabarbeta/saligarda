# ==============================================================================
# 03_build_ajuntament.R -- Registre diari de l'Ajuntament de la Garriga
#   "1. El Temps Ajunt LG (des de Marc 2002).xls": un full per any (2002-2024),
#   12 blocs mensuals per full. Cada bloc: T max/min, sensacio, amplitud,
#   ratxa maxima (km/h) i la seva DIRECCIO EN GRAUS, pluja, HR max/min,
#   pressio max/min.
#   Aporta 9 anys de vent (2002-2010) previs a la serie de 5 min.
#   Sortida: derived/la_garriga_diari_ajuntament.csv
# ==============================================================================
suppressMessages({library(readxl); library(data.table)})
source("config_direccio.R")

f <- "Saligarda/Dades/1. El Temps Ajunt LG (des de Març 2002).xls"
stopifnot(file.exists(f))

# Compte: "ºC M" i "ºC m" nomes es diferencien per la caixa -> no es pot passar
# a minuscules. Des del 2019 la columna de direccio passa de "Dir" a "Dir º".
VARS <- c("ºC M" = "tmax", "ºC m" = "tmin", "ºC sen" = "wchill_min",
          "Ampl." = "amplitud", "km/h" = "ratxa_max",
          "Dir" = "ratxa_dir", "Dir º" = "ratxa_dir", "Dirº" = "ratxa_dir",
          "Dir °" = "ratxa_dir", "Dir°" = "ratxa_dir",
          "l/m2" = "pluja", "% M" = "hr_max", "% m" = "hr_min",
          "mb M" = "p_max", "mb m" = "p_min")

fulls <- excel_sheets(f)
anys  <- fulls[grepl("^(19|20)[0-9]{2}$", fulls)]
cat("Fulls anuals:", paste(anys, collapse = " "), "\n")

llegeix_any <- function(sh) {
  x <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE,
                                   col_types = "text", .name_repair = "minimal"))
  x <- as.data.frame(x)
  cap <- trimws(as.character(unlist(x[2, ])))
  ini <- which(cap == "ºC M")                      # inici de cada bloc mensual
  if (length(ini) == 0) return(NULL)
  fi  <- c(ini[-1] - 1, ncol(x))
  dades <- x[-c(1, 2), , drop = FALSE]
  dia <- suppressWarnings(as.integer(dades[[1]]))

  out <- vector("list", length(ini))
  for (k in seq_along(ini)) {
    cols <- ini[k]:fi[k]
    nm   <- VARS[cap[cols]]
    cols <- cols[!is.na(nm)]
    nm   <- nm[!is.na(nm)]
    if (!length(cols)) next
    b <- as.data.table(dades[, cols, drop = FALSE])
    setnames(b, unname(nm))
    b[, `:=`(any = as.integer(sh), mes = k, dia = dia)]
    out[[k]] <- b
  }
  rbindlist(out, fill = TRUE)
}

d <- rbindlist(lapply(anys, llegeix_any), fill = TRUE)

# "ip" = inapreciable (traca de pluja) -> 0
d[, pluja := fifelse(trimws(tolower(pluja)) %in% c("ip", "inap", "-", "."), "0", pluja)]
for (cn in unname(VARS)) {
  if (cn %in% names(d)) d[, (cn) := suppressWarnings(as.numeric(get(cn)))]
}

d <- d[!is.na(dia) & dia >= 1 & dia <= 31 & !is.na(mes)]
d[, date := as.Date(sprintf("%04d-%02d-%02d", any, mes, dia), optional = TRUE)]
d <- d[!is.na(date)]
setorder(d, date)
d <- unique(d, by = "date")

# control de qualitat
d[tmax < -25 | tmax > 48, tmax := NA]
d[tmin < -25 | tmin > 48, tmin := NA]
d[ratxa_max < 0 | ratxa_max > 200, ratxa_max := NA]
d[hr_max < 0 | hr_max > 100, hr_max := NA]
d[hr_min < 0 | hr_min > 100, hr_min := NA]
d[p_max < 940 | p_max > 1060, p_max := NA]
d[p_min < 940 | p_min > 1060, p_min := NA]
d[pluja < 0 | pluja > 400, pluja := NA]
# direccio: graus 0-360 (la taula del full usa 0, 22/23, 45, 67/68, 90, ...)
d[ratxa_dir < 0 | ratxa_dir > 360, ratxa_dir := NA]
d[ratxa_dir == 360, ratxa_dir := 0]

# --------------------------- correccio de l'orientacio de la penella
# Fins al 2008 l'aparell estava ben orientat (coincideix amb el terreny a 0,5
# graus); des del 2011 les direccions estan girades. 2009-2010 es la transicio
# i es marquen com a poc fiables.
d[, ratxa_dir_bruta := ratxa_dir]
d[any >= ANY_INTERVENCIO, ratxa_dir := corregeix_direccio(ratxa_dir)]
d[, dir_fiable := !(any %in% ANYS_TRANSICIO)]

# la cel.la de pluja es deixa en blanc els dies secs: 0 si el dia es va registrar
d[is.na(pluja) & !is.na(tmax), pluja := 0]

d <- d[, .(date, any, mes, dia, tmax, tmin, amplitud, wchill_min,
           ratxa_max, ratxa_dir, ratxa_dir_bruta, dir_fiable,
           pluja, hr_max, hr_min, p_max, p_min)]

if (!dir.exists("derived")) dir.create("derived")
fwrite(d, "derived/la_garriga_diari_ajuntament.csv")

cat("\n-> derived/la_garriga_diari_ajuntament.csv:", nrow(d), "dies\n")
cat("   periode:", format(min(d$date)), "->", format(max(d$date)), "\n\n")
print(d[, .(dies = .N, tmax = sum(!is.na(tmax)), ratxa = sum(!is.na(ratxa_max)),
            dir = sum(!is.na(ratxa_dir)), pluja = sum(!is.na(pluja)),
            p = sum(!is.na(p_max))), by = any][order(any)])
cat("\n--- direccions de la ratxa maxima diaria (graus, top 20) ---\n")
print(head(sort(table(d$ratxa_dir), decreasing = TRUE), 20))
