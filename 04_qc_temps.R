# ==============================================================================
# 04_qc_temps.R -- Control de qualitat de la referencia horaria
#   1) La Davis de la Garriga, marca hora civil (amb canvi d'hora) o fixa?
#      Test: hora del minim diari de temperatura abans i despres del canvi.
#   2) Quin desfasament hi ha entre la Garriga i Vic (XEMA publica en UTC)?
#      Test: correlacio creuada de les series de temperatura a 30 min.
#   Sortida: figures/qc_hora.png i diagnostic per pantalla
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

gar <- fread("derived/la_garriga_5min.csv")
gar[, datetime := as.POSIXct(datetime, tz = "UTC")]   # llegida "tal qual", sense TZ
vic <- fread("vic.csv")
vic[, datetime := as.POSIXct(datetime, tz = "UTC")]

# ---------------------------------------------------------------- 1) canvi d'hora
gar[, `:=`(date = as.IDate(datetime), h = hour(datetime) + minute(datetime) / 60)]
tmin_h <- gar[!is.na(temp_out), .SD[which.min(temp_out)], by = date][, .(date, h_tmin = h)]
tmax_h <- gar[!is.na(temp_out), .SD[which.max(temp_out)], by = date][, .(date, h_tmax = h)]
ext <- merge(tmin_h, tmax_h, by = "date")
ext[, `:=`(any = year(date), doy = yday(date))]

# mitjana movil de l'hora del maxim: si la marca es civil, hi ha un esglao
# d'una hora just als canvis d'hora (ultim diumenge de marc i d'octubre)
mens <- ext[, .(h_tmin = median(h_tmin, na.rm = TRUE),
                h_tmax = median(h_tmax, na.rm = TRUE),
                n = .N), by = .(mes = month(date))]
cat("=== Hora mediana del maxim i el minim diaris, per mes (marca original) ===\n")
print(mens)

# comparacio directa: setmanes just abans / just despres del canvi de marc
canvi_marc <- as.IDate(c("2013-03-31","2014-03-30","2015-03-29","2016-03-27",
                         "2017-03-26","2018-03-25","2019-03-31","2020-03-29",
                         "2021-03-28","2022-03-27","2023-03-26"))
abans <- ext[date %in% unlist(lapply(canvi_marc, function(x) x - (1:10)))]
despr <- ext[date %in% unlist(lapply(canvi_marc, function(x) x + (1:10)))]
cat("\n=== Canvi d'hora de marc (10 dies abans vs 10 dies despres) ===\n")
cat(sprintf("  hora mediana del Tmax  abans: %.2f   despres: %.2f   salt: %+.2f h\n",
            median(abans$h_tmax, na.rm = TRUE), median(despr$h_tmax, na.rm = TRUE),
            median(despr$h_tmax, na.rm = TRUE) - median(abans$h_tmax, na.rm = TRUE)))
cat(sprintf("  hora mediana del Tmin  abans: %.2f   despres: %.2f   salt: %+.2f h\n",
            median(abans$h_tmin, na.rm = TRUE), median(despr$h_tmin, na.rm = TRUE),
            median(despr$h_tmin, na.rm = TRUE) - median(abans$h_tmin, na.rm = TRUE)))
cat("  (un salt proper a +1 h indica marca horaria CIVIL, amb canvi d'hora)\n")

# ------------------------------------------------- 2) desfasament Garriga vs Vic
# la Garriga a passos de 30 min per poder comparar amb la XEMA
g30 <- gar[, .(tg = mean(temp_out, na.rm = TRUE)),
           by = .(datetime = as.POSIXct(round(as.numeric(datetime) / 1800) * 1800,
                                        origin = "1970-01-01", tz = "UTC"))]
v30 <- vic[, .(datetime, tv = T)]
both <- merge(g30, v30, by = "datetime")
both <- both[!is.na(tg) & !is.na(tv)]
cat("\n=== Correlacio creuada Garriga-Vic (temperatura, 30 min) ===\n")
cat("   registres comuns:", nrow(both), "\n")

lags <- -6:6                                  # passos de 30 min
cc <- sapply(lags, function(k) {
  x <- both$tg
  y <- shift(both$tv, k)
  suppressWarnings(cor(x, y, use = "complete.obs"))
})
res <- data.table(lag_min = lags * 30, r = round(cc, 5))
print(res)
millor <- res[which.max(r)]
cat(sprintf("\n  maxima correlacio a lag = %+d min (r = %.4f)\n", millor$lag_min, millor$r))
cat("  lag 0 => les dues series ja son a la mateixa referencia\n")
cat("  lag != 0 => cal desplacar la Garriga abans de creuar-la amb Vic\n")

# el mateix pero nomes a l'hivern i nomes a l'estiu (el desfasament civil-UTC
# canvia una hora entre les dues estacions de l'any si la marca es civil)
for (et in list(list("hivern (DJF)", c(12,1,2)), list("estiu (JJA)", c(6,7,8)))) {
  b <- both[month(datetime) %in% et[[2]]]
  cc <- sapply(lags, function(k) suppressWarnings(cor(b$tg, shift(b$tv, k), use = "complete.obs")))
  k <- lags[which.max(cc)] * 30
  cat(sprintf("  %-14s: lag optim = %+d min (r = %.4f, n = %d)\n",
              et[[1]], k, max(cc), nrow(b)))
}

if (!dir.exists("figures")) dir.create("figures")
p <- ggplot(ext[any >= 2012], aes(doy, h_tmax)) +
  geom_point(alpha = 0.06, size = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 30), colour = "#C0392B") +
  geom_vline(xintercept = c(87, 300), linetype = 2, colour = "#2980B9") +
  scale_x_continuous("Dia de l'any", breaks = seq(0, 360, 30)) +
  scale_y_continuous("Hora del maxim diari de T", breaks = seq(8, 20, 1)) +
  labs(title = "Hora del maxim diari de temperatura a la Garriga",
       subtitle = "Linies blaves: canvis d'hora (finals de marc i d'octubre)") +
  theme_bw()
ggsave("figures/qc_hora.png", p, width = 8, height = 4.5, dpi = 150)
cat("\n-> figures/qc_hora.png\n")
