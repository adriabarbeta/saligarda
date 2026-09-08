# ==============================================================================
# 13_direccio_vs_vall.R -- Per que 22 graus i no la direccio de la vall?
#   L'eix del Congost aigues amunt de la Garriga va cap al NNO, pero el vent
#   nocturn mesurat ve del NNE. Son uns 45 graus de diferencia, i cal mirar si
#   es geometria (dos fluxos superposats), instrumental (penella desalineada) o
#   una barreja.
#   Proves: (a) resolucio real de la direccio al full de l'Ajuntament,
#           (b) direccio segons la intensitat, (c) segons l'hora, (d) segons
#           l'estacio de l'any.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

# ------------------------------------------- eix de la vall segons coordenades
# coordenades aproximades dels nuclis del fons de vall, aigues amunt
pobles <- data.table(
  lloc = c("la Garriga", "el Figaro", "Aiguafreda", "Centelles", "Vic"),
  lat  = c(41.6833, 41.7200, 41.7683, 41.7975, 41.9350),
  lon  = c(2.2833,  2.2750,  2.2500,  2.2200,  2.2398))
rumb <- function(lat1, lon1, lat2, lon2) {          # rumb de 1 cap a 2
  dx <- (lon2 - lon1) * 111.320 * cos((lat1 + lat2)/2 * pi/180)
  dy <- (lat2 - lat1) * 111.320
  list(rumb = (atan2(dx, dy) * 180/pi) %% 360, dist = sqrt(dx^2 + dy^2))
}
cat("=== Rumb des de la Garriga cap amunt de la vall (d'on hauria de venir l'aire) ===\n")
for (i in 2:nrow(pobles)) {
  r <- rumb(pobles$lat[1], pobles$lon[1], pobles$lat[i], pobles$lon[i])
  cat(sprintf("  cap a %-12s %5.1f graus  (%4.1f km)\n", pobles$lloc[i], r$rumb, r$dist))
}
cat("  [coordenades aproximades dels nuclis: serveixen per a l'ordre de magnitud]\n")
cat("\n  direccio mitjana vectorial observada del vent nocturn d'hivern: 22,9 graus\n")

# --------------------- (a) quina resolucio real te la direccio de l'Ajuntament?
# aquest diagnostic ha de mirar la mesura CRUA, no la corregida
A <- fread("derived/la_garriga_diari_ajuntament.csv")
A <- A[!is.na(ratxa_dir_bruta)][, ratxa_dir := ratxa_dir_bruta]
SEIZE <- round(seq(0, 337.5, by = 22.5))            # 0 22 45 68 90 ... arrodonits
A[, es_sector := ratxa_dir %in% c(SEIZE, SEIZE + 1, 158, 203, 248, 293, 338)]
cat("\n=== (a) Resolucio de la direccio al full de l'Ajuntament ===\n")
print(A[, .(dies = .N, frac_en_rumbs_de_16 = round(mean(es_sector), 3),
            valors_diferents = uniqueN(ratxa_dir)), by = any][order(any)])
cat("  Si en algun periode la fraccio baixa, es que s'hi va anotar en graus reals.\n")

# ------------------------------------ (b,c,d) estructura fina amb la serie 5 min
g <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
g[, est := factor(est, levels = c("DJF","MAM","JJA","SON"))]
dirmit <- function(deg, w = NULL) {                 # direccio mitjana vectorial
  ok <- !is.na(deg); deg <- deg[ok]
  if (!length(deg)) return(NA_real_)
  (atan2(mean(sin(deg*pi/180)), mean(cos(deg*pi/180))) * 180/pi) %% 360
}
nit <- g[h < 10 & !is.na(hi_dir_deg) & !is.na(wind_mean) & wind_mean >= 2]

cat("\n=== (b) Direccio segons la intensitat (nit, 00-10 UTC) ===\n")
nit[, cls := cut(wind_mean, c(2, 5, 8, 12, 16, 20, Inf), right = FALSE,
                 labels = c("2-5","5-8","8-12","12-16","16-20",">20"))]
print(nit[, .(registres = .N, dir_mitjana = round(dirmit(hi_dir_deg), 1),
              R = round(sqrt(mean(sin(hi_dir_deg*pi/180))^2 +
                             mean(cos(hi_dir_deg*pi/180))^2), 3)),
          by = cls][order(cls)])
cat("  Si el flux fort s'acosta al rumb de la vall i el fluix no, hi ha dos fluxos.\n")

cat("\n=== (c) Direccio segons l'hora (hivern, vent >= 5 km/h) ===\n")
print(g[mes %in% c(11,12,1,2) & !is.na(hi_dir_deg) & wind_mean >= 5,
        .(registres = .N, dir_mitjana = round(dirmit(hi_dir_deg), 1),
          vel = round(mean(wind_mean), 1)), by = .(hora = floor(h))][order(hora)])

cat("\n=== (d) Direccio segons l'estacio de l'any (nit, vent >= 5 km/h) ===\n")
print(nit[wind_mean >= 5, .(registres = .N, dir_mitjana = round(dirmit(hi_dir_deg), 1)),
          by = est][order(est)])

# ------------------------------------------------------------------- figura
if (!dir.exists("figures")) dir.create("figures")
rd <- nit[, .N, by = .(cls, sect16)]
rd[, frac := 100 * N / sum(N), by = cls]
p <- ggplot(rd[!is.na(cls)], aes(sect16, frac)) +
  geom_col(fill = "#2166AC", width = 1, colour = "white", linewidth = 0.15) +
  coord_polar(start = -pi/16) +
  facet_wrap(~cls, nrow = 2) +
  scale_x_discrete(breaks = c("N","E","S","O")) +
  labs(x = NULL, y = "% dels registres de la classe",
       title = "La direccio nocturna canvia amb la intensitat?",
       subtitle = "Classes de velocitat mitjana (km/h), 00-10 UTC. Si els dos fluxos son diferents, la rosa gira.") +
  theme_bw(base_size = 9) +
  theme(axis.text.y = element_text(size = 6),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"))
ggsave("figures/F22_direccio_per_intensitat.png", p, width = 8, height = 5.5, dpi = 150)
cat("\n-> figures/F22_direccio_per_intensitat.png\n")
