# ==============================================================================
# 06_index_saligarda.R -- Definicio objectiva d'un episodi de Saligarda
#   Index diari = mitjana de la component vall avall (u_dv) entre 00 i 10 UTC.
#   Es descriu la distribucio, es tria el llindar, es classifiquen els dies i
#   es separa el drenatge pur del vent del nord d'origen sinoptic.
#   Sortida: derived/saligarda_diari.csv
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})
SECTOR_VALL <- c("NNO", "N", "NNE")   # tres rumbs centrats a l'eix corregit

g <- fread("derived/garriga_treball.csv")
g[, datetime := as.POSIXct(datetime, tz = "UTC")]
g[, date := as.IDate(date)]
g[, est := factor(est, levels = c("DJF", "MAM", "JJA", "SON"))]

H_INI <- 0; H_FI <- 10          # finestra nocturna/matinal (UTC)

# ------------------------------------------------- metriques nocturnes per dia
nit <- g[h >= H_INI & h < H_FI]
dia <- nit[, .(
  n_obs   = .N,
  n_vent  = sum(!is.na(u_dv)),
  u_dv    = mean(u_dv, na.rm = TRUE),                 # index principal
  u_dv_max= suppressWarnings(max(u_dv, na.rm = TRUE)),
  v_mitja = mean(wind_mean, na.rm = TRUE),
  ratxa_max = suppressWarnings(max(hi_speed, na.rm = TRUE)),
  # constancia direccional: |R| del vector unitari mitja
  R = { s <- sin(hi_dir_deg * pi/180); c <- cos(hi_dir_deg * pi/180)
        ok <- !is.na(s) & !is.na(wind_mean) & wind_mean >= 2
        if (sum(ok) < 10) NA_real_ else sqrt(mean(s[ok])^2 + mean(c[ok])^2) },
  frac_N  = mean(sect16 %in% SECTOR_VALL, na.rm = TRUE),
  t_min   = min(temp_out, na.rm = TRUE),
  wc_min  = suppressWarnings(min(wc, na.rm = TRUE)),        # sensacio de fred minima
  wc_mitja= mean(wc, na.rm = TRUE),
  wc_gap  = mean(wc_gap, na.rm = TRUE),                     # rebaixa deguda al vent
  hr_mitja= mean(out_hum, na.rm = TRUE),
  bar_mitja = mean(bar, na.rm = TRUE),
  pluja   = sum(rain, na.rm = TRUE)
), by = .(date, any, mes, est)]
for (cn in c("u_dv_max","ratxa_max","t_min","wc_min")) set(dia, which(is.infinite(dia[[cn]])), cn, NA)
dia <- dia[n_vent >= 100]                    # >= 100 de 120 registres de 5 min

# component vall avall de la tarda: distingeix drenatge pur de vent sinoptic
tarda <- g[h >= 13 & h < 19, .(u_dv_tarda = mean(u_dv, na.rm = TRUE),
                               v_tarda = mean(wind_mean, na.rm = TRUE)), by = date]
dia <- merge(dia, tarda, by = "date", all.x = TRUE)

cat("=== Dies amb dades nocturnes suficients:", nrow(dia), "===\n")
cat("   periode:", format(min(dia$date)), "->", format(max(dia$date)), "\n")

# ------------------------------------------------------ distribucio de l'index
cat("\n=== Distribucio de l'index u_dv (00-10 UTC, km/h) ===\n")
print(round(quantile(dia$u_dv, c(.05,.1,.25,.5,.75,.9,.95,.99,1), na.rm = TRUE), 2))
cat("\n  per estacio de l'any:\n")
print(dia[, .(n = .N, p25 = round(quantile(u_dv, .25, na.rm=TRUE),1),
              mediana = round(median(u_dv, na.rm=TRUE),1),
              p75 = round(quantile(u_dv, .75, na.rm=TRUE),1),
              p95 = round(quantile(u_dv, .95, na.rm=TRUE),1),
              max = round(max(u_dv, na.rm=TRUE),1)), by = est][order(est)])

# ------------------------------------------------------------- criteri adoptat
# Dia de Saligarda: flux mitja vall avall >= 8 km/h entre 00 i 10 UTC
# i direccio consistent (|R| >= 0,7). El llindar es contrasta mes avall.
LLINDAR <- 8; R_MIN <- 0.7
dia[, saligarda := !is.na(u_dv) & u_dv >= LLINDAR & !is.na(R) & R >= R_MIN]

cat(sprintf("\n=== Criteri: u_dv >= %.0f km/h i |R| >= %.1f ===\n", LLINDAR, R_MIN))
cat(sprintf("  dies de Saligarda: %d de %d (%.1f%%)\n",
            sum(dia$saligarda), nrow(dia), 100 * mean(dia$saligarda)))

cat("\n  sensibilitat al llindar (dies/any):\n")
for (L in c(5, 6, 8, 10, 12, 15)) {
  s <- dia[, .(n = sum(u_dv >= L & R >= R_MIN, na.rm = TRUE)), by = any][any %in% 2012:2023]
  cat(sprintf("   u_dv >= %2d km/h : %5.1f dies/any (rang %d-%d)\n",
              L, mean(s$n), min(s$n), max(s$n)))
}

# --------------------------------------------- drenatge pur vs vent sinoptic
# el drenatge pur s'atura al mati; el vent del nord sinoptic continua a la tarda
dia[saligarda == TRUE, tipus := fifelse(u_dv_tarda < 3, "drenatge pur",
                                fifelse(u_dv_tarda >= 8, "nord sinoptic",
                                        "intermedi"))]
cat("\n=== Tipus d'episodi ===\n")
print(dia[saligarda == TRUE, .N, by = tipus][order(-N)])
print(dcast(dia[saligarda == TRUE, .N, by = .(est, tipus)], est ~ tipus, value.var = "N"))

# ------------------------------------------------- inici, final i hora del pic
# La nit "D" va de les 14 UTC del dia D-1 a les 13 UTC del dia D, de manera que
# l'hora d'inici es pot situar abans de mitjanit (hores negatives).
g[, hh := floor(h)]
g[, nit_date := date + fifelse(hh >= 14, 1L, 0L)]
g[, h_nit := fifelse(hh >= 14, hh - 24, hh)]        # -10 .. 13
horari <- g[!is.na(u_dv), .(u = mean(u_dv)), by = .(nit_date, h_nit)]
llind_ep <- 5
ep <- horari[nit_date %in% dia[saligarda == TRUE, date]][order(nit_date, h_nit), {
  sobre <- h_nit[u >= llind_ep]
  .(inici = if (length(sobre)) min(sobre) else NA_real_,
    final = if (length(sobre)) max(sobre) else NA_real_,
    pic   = h_nit[which.max(u)],
    u_pic = max(u))
}, by = .(date = nit_date)]
dia <- merge(dia, ep, by = "date", all.x = TRUE)
cat("\n=== Hores caracteristiques dels episodis (UTC; +1 h = hora civil d'hivern) ===\n")
print(dia[saligarda == TRUE, .(n = .N,
        inici = round(median(inici, na.rm=TRUE),1),
        pic   = round(median(pic, na.rm=TRUE),1),
        final = round(median(final, na.rm=TRUE),1),
        durada= round(median(final - inici, na.rm=TRUE),1)), by = est][order(est)])

fwrite(dia, "derived/saligarda_diari.csv")
cat("\n-> derived/saligarda_diari.csv\n")

# --------------------------------------------------------------------- figura
if (!dir.exists("figures")) dir.create("figures")
p <- ggplot(dia, aes(u_dv)) +
  geom_histogram(binwidth = 1, fill = "grey75", colour = "white", linewidth = .2) +
  geom_vline(xintercept = LLINDAR, colour = "#B2182B", linetype = 2) +
  facet_wrap(~est, scales = "free_y") +
  labs(x = "Component mitjana vall avall 00-10 UTC (km/h)", y = "dies",
       title = "Distribucio de l'index diari de Saligarda",
       subtitle = "Linia vermella: llindar adoptat (8 km/h)") +
  theme_bw(base_size = 10)
ggsave("figures/F4_distribucio_index.png", p, width = 8, height = 5, dpi = 150)
cat("-> figures/F4_distribucio_index.png\n")
