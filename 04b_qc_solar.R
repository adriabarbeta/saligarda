# ==============================================================================
# 04b_qc_solar.R -- A quina referencia horaria son les dues series?
#   Test A: hora del maxim d'irradiancia a Vic (XEMA) vs migdia solar calculat.
#           Determina si la XEMA publica en UTC o en hora civil.
#   Test B: desfasament entre l'hora del minim diari de T a la Garriga i la
#           sortida del sol calculada. Si la marca es CIVIL, el desfasament
#           salta 1 h als canvis d'hora; si es FIXA, es manté pla tot l'any.
# ==============================================================================
suppressMessages({library(data.table); library(suncalc); library(ggplot2)})

LAT_VIC <- 41.93498; LON_VIC <- 2.23984
LAT_GAR <- 41.6833;  LON_GAR <- 2.2833     # la Garriga, ~250 m

# ------------------------------------------------------------------ Test A: Vic
vic <- fread("vic.csv")
vic[, datetime := as.POSIXct(datetime, tz = "UTC")]
rs <- vic[!is.na(RS) & RS > 0]
rs[, date := as.IDate(datetime)]
# nomes dies clars: irradiancia diaria alta i sense pluja
dia <- rs[, .(rs_tot = sum(RS), pmax_h = .SD[which.max(RS), hour(datetime) + minute(datetime)/60],
              ppt = sum(PPT, na.rm = TRUE)), by = date]
clars <- dia[ppt == 0 & rs_tot > quantile(rs_tot, 0.75)]
sp <- as.data.table(getSunlightTimes(date = as.Date(clars$date),
                                     lat = LAT_VIC, lon = LON_VIC, tz = "UTC"))
clars[, migdia_solar := hour(sp$solarNoon) + minute(sp$solarNoon)/60]
cat("=== TEST A - Vic (XEMA): hora del maxim d'irradiancia en dies clars ===\n")
cat(sprintf("  n dies clars                 : %d\n", nrow(clars)))
cat(sprintf("  hora mediana del maxim de RS : %.2f h (marca del fitxer)\n",
            median(clars$pmax_h)))
cat(sprintf("  migdia solar mediana (UTC)   : %.2f h\n", median(clars$migdia_solar)))
cat(sprintf("  diferencia                   : %+.2f h\n",
            median(clars$pmax_h) - median(clars$migdia_solar)))
cat("  ~0 h => la XEMA publica en UTC | ~+1 h => CET | ~+2 h => CEST\n")
cat("\n  per estacions (per detectar canvi d'hora):\n")
clars[, est := fifelse(month(date) %in% c(12,1,2), "hivern",
               fifelse(month(date) %in% c(6,7,8), "estiu", "resta"))]
print(clars[, .(n = .N, dif = round(median(pmax_h - migdia_solar), 3)), by = est])

# ------------------------------------------------------------- Test B: la Garriga
gar <- fread("derived/la_garriga_5min.csv",
             select = c("datetime", "temp_out"))
gar[, datetime := as.POSIXct(datetime, tz = "UTC")]
gar[, date := as.IDate(datetime)]
tmin <- gar[!is.na(temp_out), .SD[which.min(temp_out)], by = date]
tmin[, h_tmin := hour(datetime) + minute(datetime)/60]
sg <- as.data.table(getSunlightTimes(date = as.Date(tmin$date),
                                     lat = LAT_GAR, lon = LON_GAR, tz = "UTC"))
tmin[, sortida := hour(sg$sunrise) + minute(sg$sunrise)/60]
tmin[, desfas := h_tmin - sortida]
tmin[, `:=`(mes = month(date), doy = yday(date))]

cat("\n=== TEST B - la Garriga: hora del Tmin menys sortida del sol ===\n")
res <- tmin[, .(n = .N, sortida_utc = round(median(sortida), 2),
                h_tmin = round(median(h_tmin), 2),
                desfas = round(median(desfas), 2)), by = mes][order(mes)]
print(res)
cat("\n  Si la marca fos CIVIL, el desfasament baixaria ~1 h de l'hivern a l'estiu\n")
cat("  (perque a l'estiu el rellotge va 1 h mes avancat). Si es FIXA, es pla.\n")
h <- res[mes %in% c(12,1,2), median(desfas)]
e <- res[mes %in% c(6,7,8), median(desfas)]
cat(sprintf("\n  desfasament hivern: %+.2f h | estiu: %+.2f h | diferencia: %+.2f h\n",
            h, e, e - h))

if (!dir.exists("figures")) dir.create("figures")
p <- ggplot(tmin[year(date) >= 2012], aes(doy, desfas)) +
  geom_point(alpha = 0.05, size = 0.4) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 30), colour = "#C0392B") +
  geom_vline(xintercept = c(87, 300), linetype = 2, colour = "#2980B9") +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_x_continuous("Dia de l'any", breaks = seq(0, 360, 30)) +
  labs(y = "Hora del Tmin - sortida del sol (h)",
       title = "La marca horaria de la Davis de la Garriga es fixa o civil?",
       subtitle = "Una marca civil faria baixar la corba ~1 h entre les linies blaves (canvis d'hora)") +
  theme_bw()
ggsave("figures/qc_solar.png", p, width = 8, height = 4.5, dpi = 150)
cat("\n-> figures/qc_solar.png\n")
