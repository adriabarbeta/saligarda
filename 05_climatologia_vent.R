# ==============================================================================
# 05_climatologia_vent.R -- Climatologia del vent a la Garriga (5 min, 2011-2024)
#   - eix de la vall del Congost deduit de les dades
#   - roses dels vents per estacio de l'any i franja horaria
#   - cicle diari x mes de la component vall avall
#   - homogeneitat de la serie (canvis d'aparell/exposicio)
#   Totes les hores son UTC (= hora solar - 9 min a aquesta longitud).
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

source("config_direccio.R")

gar <- fread("derived/la_garriga_5min.csv")
gar[, datetime := as.POSIXct(datetime, tz = "UTC")]

# --------------------------------------- correccio de l'orientacio de la penella
# La serie de 5 min comenca el marc de 2011, ja despres de la intervencio, de
# manera que la correccio s'aplica sencera. Es guarda la mesura original.
gar[, hi_dir_deg_bruta := hi_dir_deg]
gar[, hi_dir_deg := corregeix_direccio(hi_dir_deg)]
cat(sprintf("=== Direccions corregides en %d graus (vegeu config_direccio.R) ===\n",
            OFFSET_PENELLA))
gar[, `:=`(any = year(datetime), mes = month(datetime),
           h = hour(datetime) + minute(datetime) / 60,
           date = as.IDate(datetime))]
gar[, est := fcase(mes %in% c(12, 1, 2), "DJF", mes %in% 3:5, "MAM",
                   mes %in% 6:8, "JJA", default = "SON")]
gar[, est := factor(est, levels = c("DJF", "MAM", "JJA", "SON"))]

# --------------------------------------------------------------- factor de ratxa
gf <- gar[!is.na(wind_mean) & !is.na(hi_speed) & wind_mean > 2,
          .(n = .N, ratio = median(hi_speed / wind_mean, na.rm = TRUE))]
cat("=== Relacio ratxa / mitjana (5 min) ===\n")
cat(sprintf("  mediana hi_speed / wind_mean = %.2f  (n = %d)\n", gf$ratio, gf$n))

# --------------------------------------------------- eix de la vall segons dades
# vent nocturn d'hivern amb prou forca: direccio mitjana vectorial
nit <- gar[h < 8 & mes %in% c(11, 12, 1, 2, 3) & !is.na(hi_dir_deg) & wind_mean >= 5]
u <- mean(sin(nit$hi_dir_deg * pi / 180)); v <- mean(cos(nit$hi_dir_deg * pi / 180))
eix <- (atan2(u, v) * 180 / pi) %% 360
cat("\n=== Eix de drenatge deduit de les dades ===\n")
cat(sprintf("  direccio mitjana vectorial del vent nocturn d'hivern (>=5 km/h): %.1f graus\n", eix))
cat(sprintf("  constancia direccional |R| = %.3f  (n = %d)\n", sqrt(u^2 + v^2), nrow(nit)))
# S'adopta l'eix empiric, no un valor rodo: aixi la projeccio es exactament
# invariant a la correccio de la penella (si giren les dades, gira l'eix).
EIX <- round(eix, 1)
cat(sprintf("  eix adoptat per als calculs: %.1f graus\n", EIX))
cat("  rumb geografic del Congost des de la Garriga: 350 (el Figaro), 344 (Aiguafreda)\n")

# Sector de la vall: els tres rumbs centrats a l'eix. Amb les direccions ja
# corregides son NNO-N-NNE; en brut haurien estat N-NNE-NE.
SECTOR_VALL <- c("NNO", "N", "NNE")
cat(sprintf("  sector de la vall adoptat: %s\n", paste(SECTOR_VALL, collapse = "-")))

# component vall avall (positiva = vent que baixa del nord per la vall)
gar[, u_dv := wind_mean * cos((hi_dir_deg - EIX) * pi / 180)]
gar[, u_dv_ratxa := hi_speed * cos((hi_dir_deg - EIX) * pi / 180)]

# ------------------------------------------------------------ sensacio de fred
# Formula JAG/TI (Environment Canada / NWS), valida per T <= 10 C i V >= 4,8 km/h;
# fora d'aquest rang la sensacio es la temperatura. Validada contra la columna
# "Wind Chill" de la Davis: biaix +0,04 C, error absolut mitja 0,08 C, r = 0,997.
gar[, wc := fifelse(!is.na(wind_mean) & !is.na(temp_out) &
                      wind_mean >= 4.8 & temp_out <= 10,
                    13.12 + 0.6215 * temp_out - 11.37 * wind_mean^0.16 +
                      0.3965 * temp_out * wind_mean^0.16,
                    temp_out)]
gar[, wc_gap := wc - temp_out]        # rebaixa de sensacio deguda al vent (<= 0)
cat("\n=== Sensacio de fred: validacio contra la columna de la Davis ===\n")
# nomes on la formula s'aplica de veritat (T <= 10 C i V >= 4,8 km/h); fora
# d'aquest rang wc = temperatura per definicio i la comparacio no diu res
apl <- gar[!is.na(wind_chill) & !is.na(wind_mean) & wind_mean >= 4.8 & temp_out <= 10]
cat(sprintf("  rang d'aplicacio  : n = %d | biaix = %+.3f C | EAM = %.3f C | r = %.4f\n",
            nrow(apl), mean(apl$wc - apl$wind_chill), mean(abs(apl$wc - apl$wind_chill)),
            cor(apl$wc, apl$wind_chill)))
fora <- gar[!is.na(wind_chill) & !is.na(wc) & temp_out <= 10 &
              (is.na(wind_mean) | wind_mean < 4.8)]
cat(sprintf("  fora de rang      : n = %d | biaix = %+.3f C (wc = T per definicio)\n",
            nrow(fora), mean(fora$wc - fora$wind_chill)))

# --------------------------------------------- distribucio de direccions per franja
gar[, sect16 := factor(round(hi_dir_deg / 22.5) %% 16,
                       levels = 0:15,
                       labels = c("N","NNE","NE","ENE","E","ESE","SE","SSE",
                                  "S","SSO","SO","OSO","O","ONO","NO","NNO"))]
gar[, franja := fcase(h >= 0 & h < 10, "nit i mati (00-10 UTC)",
                      h >= 12 & h < 20, "tarda (12-20 UTC)",
                      default = "resta")]
tab <- gar[!is.na(sect16) & wind_mean >= 3 & franja != "resta",
           .N, by = .(est, franja, sect16)]
tab[, frac := N / sum(N), by = .(est, franja)]
cat(sprintf("\n=== Frequencia del sector de la vall (%s), vent >= 3 km/h ===\n",
            paste(SECTOR_VALL, collapse = "-")))
print(dcast(tab[sect16 %in% SECTOR_VALL][, .(frac = sum(frac)), by = .(est, franja)],
            est ~ franja, value.var = "frac")[, lapply(.SD, function(x)
              if (is.numeric(x)) round(x, 3) else x)])

# ------------------------------------------------------------------ homogeneitat
cat("\n=== Homogeneitat de la serie de vent (per any) ===\n")
hom <- gar[, .(n = .N,
               v_mitja = round(mean(wind_mean, na.rm = TRUE), 2),
               v_p95 = round(quantile(wind_mean, 0.95, na.rm = TRUE), 1),
               ratxa_p95 = round(quantile(hi_speed, 0.95, na.rm = TRUE), 1),
               calmes = round(mean(hi_speed < 1.6, na.rm = TRUE), 3),
               dir_vall = round(mean(sect16 %in% SECTOR_VALL, na.rm = TRUE), 3)),
           by = any][order(any)]
print(hom)

# ============================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"))

# --- F1: roses dels vents estacio x franja -----------------------------------
rosa <- gar[!is.na(sect16) & wind_mean >= 1 & franja != "resta"]
rosa[, vcat := cut(wind_mean, breaks = c(0, 5, 10, 15, 20, Inf),
                   labels = c("1-5", "5-10", "10-15", "15-20", ">20"), right = FALSE)]
rd <- rosa[, .N, by = .(est, franja, sect16, vcat)]
rd[, frac := 100 * N / sum(N), by = .(est, franja)]

p1 <- ggplot(rd, aes(sect16, frac, fill = vcat)) +
  geom_col(width = 1, colour = "white", linewidth = 0.15) +
  coord_polar(start = -pi / 16) +
  facet_grid(franja ~ est) +
  scale_fill_brewer("km/h", palette = "YlGnBu", direction = 1) +
  scale_x_discrete(breaks = c("N", "E", "S", "O")) +
  labs(x = NULL, y = "% d'observacions",
       title = "Roses dels vents a la Garriga (2011-2024, dades de 5 min)",
       subtitle = paste0("Velocitat mitjana de l'interval; nomes registres amb vent ",
                         ">= 1 km/h. Hores UTC.\nDireccions corregides en -32 graus ",
                         "per la desalineacio de la penella (config_direccio.R).")) +
  tema + theme(axis.text.y = element_text(size = 6))
ggsave("figures/F1_roses_vent.png", p1, width = 10, height = 6, dpi = 150)

# --- F2: cicle diari x mes de la component vall avall ------------------------
hm <- gar[!is.na(u_dv), .(u = mean(u_dv)), by = .(mes, hh = floor(h))]
p2 <- ggplot(hm, aes(hh, factor(mes, levels = 12:1), fill = u)) +
  geom_raster() +
  scale_fill_gradient2("Component\nvall avall\n(km/h)", low = "#2166AC",
                       mid = "grey96", high = "#B2182B", midpoint = 0) +
  scale_x_continuous("Hora (UTC)", breaks = seq(0, 23, 3), expand = c(0, 0)) +
  scale_y_discrete("Mes", expand = c(0, 0)) +
  labs(title = "Component del vent al llarg de l'eix de la vall del Congost",
       subtitle = "Positiu (vermell) = vent que baixa del nord. Mitjana 2011-2024.") +
  tema
ggsave("figures/F2_cicle_diari_mes.png", p2, width = 8, height = 4.5, dpi = 150)

# --- F3: cicle diari de la velocitat per estacio de l'any --------------------
cd <- gar[!is.na(wind_mean), .(v = mean(wind_mean), u = mean(u_dv, na.rm = TRUE)),
          by = .(est, hh = floor(h))]
p3 <- ggplot(cd, aes(hh)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(aes(y = v, colour = "velocitat mitjana"), linewidth = 0.8) +
  geom_line(aes(y = u, colour = "component vall avall"), linewidth = 0.8) +
  facet_wrap(~est, nrow = 1) +
  scale_colour_manual(NULL, values = c("velocitat mitjana" = "grey30",
                                       "component vall avall" = "#B2182B")) +
  scale_x_continuous("Hora (UTC)", breaks = seq(0, 24, 6)) +
  labs(y = "km/h", title = "Cicle diari del vent a la Garriga per estacio de l'any") +
  tema + theme(legend.position = "bottom")
ggsave("figures/F3_cicle_diari_estacions.png", p3, width = 9, height = 3.5, dpi = 150)

fwrite(gar[, .(datetime, date, any, mes, est, h, temp_out, out_hum, bar, rain,
               hi_speed, wind_mean, hi_dir_deg, hi_dir_deg_bruta, sect16,
               u_dv, u_dv_ratxa, wc, wc_gap)],
       "derived/garriga_treball.csv")
cat("\n-> derived/garriga_treball.csv i figures F1-F3\n")
