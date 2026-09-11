# ==============================================================================
# 16_pronostic.R -- Pronostic operatiu de la Saligarda per a la nit vinent
#
#   S'executa cada vespre (recomanat: 19:00 hora local, que tant a l'hivern com
#   a l'estiu deixa tancada la finestra de 12-17 UTC del dia en curs).
#
#   Que fa:
#     1. Baixa el pronostic d'Open-Meteo dels quatre punts
#     2. Construeix els predictors amb la MATEIXA funcio de l'entrenament
#     3. Aplica els models desats a derived/model_operatiu.rds
#     4. Escriu el butlleti (pronostic.txt i pronostic.html) i apunta la
#        prediccio a derived/pronostics.csv per poder-la verificar despres
#
#   Us:  Rscript 16_pronostic.R            (nit vinent)
#        Rscript 16_pronostic.R 2026-01-15 (una nit concreta, si es a l'abast)
# ==============================================================================
suppressMessages({library(jsonlite); library(data.table); library(ranger)})
`%||%` <- function(a, b) if (is.null(a)) b else a

# situa't a la carpeta del script, sigui d'on sigui que el cridin
ruta <- grep("--file=", commandArgs(FALSE), value = TRUE)
if (length(ruta)) setwd(dirname(normalizePath(sub("--file=", "", ruta[1]))))

args <- commandArgs(trailingOnly = TRUE)
MODEL <- readRDS("derived/model_operatiu.rds")

PUNTS <- data.table(nom = c("garriga","nord","sud","oest"),
                    lat = c(41.6833, 45.00, 40.50, 41.65),
                    lon = c( 2.2833,  2.00,  3.00, -0.90))
VARS <- c("temperature_2m","relative_humidity_2m","dew_point_2m","pressure_msl",
          "cloud_cover","cloud_cover_low","cloud_cover_mid","cloud_cover_high",
          "wind_speed_10m","wind_direction_10m","shortwave_radiation")

baixa_pronostic <- function(lat, lon) {
  u <- sprintf(paste0("https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
                      "&hourly=%s&past_days=2&forecast_days=3&timezone=UTC"),
               lat, lon, paste(VARS, collapse = ","))
  for (i in 1:5) {
    r <- try(suppressWarnings(fromJSON(u)), silent = TRUE)
    if (!inherits(r, "try-error") && !is.null(r$hourly)) return(as.data.table(r$hourly))
    Sys.sleep(10 * i)
  }
  stop("Open-Meteo no respon despres de 5 intents")
}

cat("Baixant pronostic ...\n")
llista <- lapply(seq_len(nrow(PUNTS)), function(i) {
  d <- baixa_pronostic(PUNTS$lat[i], PUNTS$lon[i])
  d[, datetime := as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "UTC")][, time := NULL]
  setnames(d, setdiff(names(d), "datetime"),
           paste0(PUNTS$nom[i], "_", setdiff(names(d), "datetime")))
  unique(d, by = "datetime")
})
OM <- Reduce(function(a, b) merge(a, b, by = "datetime", all = TRUE), llista)
setorder(OM, datetime)

P <- MODEL$fes_predictors(OM, clim_rad = MODEL$clim_rad)
P <- P[complete.cases(P[, MODEL$vars, with = FALSE])]

ara <- Sys.time()
attr(ara, "tzone") <- "UTC"
# La finestra de l'episodi va de les 00 a les 10 UTC del dia D. Si ja han passat
# les 10 UTC, la nit d'avui s'ha acabat i el pronostic ha de ser per a dema.
nit <- if (length(args) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", args[1])) {
  as.IDate(args[1])
} else {
  avui <- as.IDate(format(ara, "%Y-%m-%d"))
  objectiu <- if (as.integer(format(ara, "%H")) >= 10) avui + 1L else avui
  if (objectiu %in% P$date) objectiu else max(P$date)
}
p <- P[date == nit]
if (!nrow(p)) stop("no hi ha predictors per a la nit ", format(nit))

int <- predict(MODEL$intensitat, p)$predictions
pro <- predict(MODEL$ocurrencia, p)$predictions[, "1"]        # u_dv >= 8 (estudi)
wc  <- predict(MODEL$sensacio,   p)$predictions
# El bot publica el tram de 6 a 11 hora local, que es el que viu la gent, amb
# el seu propi model: entrenar sobre el mati prediu el mati millor que fer
# servir el model nocturn.
pro_bot <- if (!is.null(MODEL$ocurrencia_bot))
  predict(MODEL$ocurrencia_bot, p)$predictions[, "1"] else NA_real_
int_bot <- if (!is.null(MODEL$intensitat_bot))
  predict(MODEL$intensitat_bot, p)$predictions else NA_real_
# tram fort del mati (6-9 local): a l'estiu el drenatge ja s'ha apagat a les 9
# i la mitjana de 6-11 amaga que a primera hora si que bufava
int_1a <- if (!is.null(MODEL$intensitat_primera))
  predict(MODEL$intensitat_primera, p)$predictions else NA_real_
# cicle horari tipic de l'estacio de l'any corresponent (hores UTC)
est_nit <- c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA",
             "SON","SON","SON","DJF")[as.integer(format(nit, "%m"))]
cic <- if (!is.null(MODEL$cicle)) MODEL$cicle[est == est_nit] else NULL
pic_utc   <- if (!is.null(cic) && nrow(cic)) cic$pic[1]   else NA_integer_
final_utc <- if (!is.null(cic) && nrow(cic)) cic$final[1] else NA_integer_

categoria <- if (int >= 15) "FORTA" else if (int >= 11) "notable" else
             if (int >= 8) "moderada" else if (int >= 5) "fluixa" else "practicament nul.la"
confianca <- if (pro >= 0.8 || pro <= 0.2) "alta" else if (pro >= 0.65 || pro <= 0.35) "mitjana" else "baixa"

linies <- c(
  "==========================================================",
  sprintf(" SALIGARDA - pronostic per a la nit del %s", format(nit, "%d/%m/%Y")),
  sprintf(" emes el %s UTC", format(ara, "%Y-%m-%d %H:%M")),
  "==========================================================",
  "",
  sprintf("  Probabilitat d'episodi ....... %3.0f %%   (confianca %s)", 100*pro, confianca),
  sprintf("  MATI (%s):", MODEL$finestra_bot %||% "6-11 hora local"),
  sprintf("    probabilitat (>= %d km/h) . %3.0f %%", MODEL$llindar_bot %||% 14, 100*pro_bot),
  sprintf("    intensitat esperada ....... %4.1f km/h", int_bot),
  sprintf("  Intensitat esperada .......... %4.1f km/h  -> %s", int, categoria),
  sprintf("  Sensacio de fred minima ...... %4.1f C", wc),
  "",
  "  Condicions previstes (00-10 UTC):",
  sprintf("    nuvolositat total .......... %3.0f %%", p$om_nub),
  sprintf("    nuvolositat baixa .......... %3.0f %%", p$om_nub_baixa),
  sprintf("    pressio .................... %6.1f hPa", p$om_pmsl),
  sprintf("    tendencia 24 h ............. %+5.1f hPa", p$om_dp24),
  sprintf("    gradient nord-sud .......... %+5.1f hPa", p$om_dpNS),
  sprintf("    temperatura del model ...... %4.1f C", p$om_t2m),
  "",
  sprintf("  [model entrenat el %s amb %d nits; en validacio any a any:",
          format(MODEL$entrenat), MODEL$n_nits),
  sprintf("   intensitat R2 = %.2f, ocurrencia AUC = %.2f, sensacio RMSE = %.1f C]",
          MODEL$destresa$intensitat_R2, MODEL$destresa$ocurrencia_AUC,
          MODEL$destresa$sensacio_RMSE),
  "  Hores UTC: suma 1 h a l'hivern i 2 h a l'estiu per a hora local.",
  "==========================================================")
cat("\n"); cat(linies, sep = "\n"); cat("\n")
writeLines(linies, "pronostic.txt")

# --- registre per poder verificar despres -----------------------------------
reg <- data.table(emes = format(ara, "%Y-%m-%d %H:%M"), nit = as.character(nit),
                  p_saligarda = round(pro, 3), p_bot = round(pro_bot, 3),
                  u_mati_pred = round(int_bot, 2), u_1a_pred = round(int_1a, 2),
                  pic_utc = pic_utc, final_utc = final_utc,
                  u_dv_pred = round(int, 2),
                  wc_min_pred = round(wc, 2), categoria = categoria,
                  nub = round(p$om_nub), nub_baixa = round(p$om_nub_baixa),
                  pmsl = round(p$om_pmsl, 1), dp24 = round(p$om_dp24, 1),
                  dpNS = round(p$om_dpNS, 1))
# Si el joc de columnes ha canviat (per exemple en afegir p_bot), no es pot
# seguir afegint files al mateix fitxer: es guarda l'antic i es comenca de nou.
REG <- "derived/pronostics.csv"
if (file.exists(REG)) {
  antic <- names(fread(REG, nrows = 0))
  if (!identical(antic, names(reg))) {
    nou_nom <- sub("[.]csv$", sprintf("_fins_%s.csv", format(Sys.Date(), "%Y%m%d")), REG)
    file.rename(REG, nou_nom)
    cat("Esquema del registre canviat; l'anterior s'ha desat a", nou_nom, "\n")
  }
}
fwrite(reg, REG, append = file.exists(REG))

# --- butlleti HTML -----------------------------------------------------------
col <- if (pro >= 0.66) "#B2182B" else if (pro >= 0.33) "#E08214" else "#2166AC"
html <- sprintf('<!doctype html><meta charset="utf-8"><title>Saligarda %s</title>
<style>body{font-family:system-ui,sans-serif;max-width:34rem;margin:2rem auto;color:#222}
h1{font-size:1.2rem;margin:0 0 .2rem} .p{font-size:3.4rem;font-weight:700;color:%s;line-height:1}
table{border-collapse:collapse;margin-top:1rem;width:100%%}
td{padding:.3rem .5rem;border-bottom:1px solid #eee} td:last-child{text-align:right;font-variant-numeric:tabular-nums}
small{color:#666}</style>
<h1>Saligarda &middot; nit del %s</h1>
<small>Emes el %s UTC</small>
<p class="p">%.0f%%</p>
<p><strong>%s</strong> &middot; intensitat esperada %.1f km/h &middot; sensacio minima %.1f &deg;C</p>
<table>
<tr><td>Nuvolositat total (00-10 UTC)</td><td>%.0f %%</td></tr>
<tr><td>Nuvolositat baixa</td><td>%.0f %%</td></tr>
<tr><td>Pressio</td><td>%.1f hPa</td></tr>
<tr><td>Tendencia 24 h</td><td>%+.1f hPa</td></tr>
<tr><td>Gradient nord-sud</td><td>%+.1f hPa</td></tr>
</table>
<p><small>Model entrenat amb %d nits. Validacio any a any: R&sup2; = %.2f (intensitat),
AUC = %.2f (ocurrencia), RMSE = %.1f &deg;C (sensacio).</small></p>',
  format(nit), col, format(nit, "%d/%m/%Y"), format(ara, "%Y-%m-%d %H:%M"),
  100*pro, categoria, int, wc, p$om_nub, p$om_nub_baixa, p$om_pmsl, p$om_dp24, p$om_dpNS,
  MODEL$n_nits, MODEL$destresa$intensitat_R2, MODEL$destresa$ocurrencia_AUC,
  MODEL$destresa$sensacio_RMSE)
writeLines(html, "pronostic.html")
cat("-> pronostic.txt, pronostic.html i derived/pronostics.csv\n")
