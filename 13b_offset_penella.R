# ==============================================================================
# 13b_offset_penella.R -- La penella esta desalineada?
#   Dues proves independents:
#   (1) El full de l'Ajuntament anota la direccio en GRAUS REALS fins al 2010 i
#       en rumbs de 16 a partir del 2012 (vegeu 13_). Si abans del 2011 el vent
#       nocturn del nord venia del NNO i despres del NNE, hi ha hagut un canvi
#       d'aparell o d'orientacio, no un canvi de vent.
#   (2) En una vall ben canalitzada el flux vall avall i el flux vall amunt han
#       de ser gairebe antiparal.lels (180 graus). Si no ho son, la referencia
#       de la penella no quadra amb el terreny.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

dirmit <- function(deg) {
  deg <- deg[!is.na(deg)]
  if (!length(deg)) return(c(dir = NA, R = NA))
  s <- mean(sin(deg*pi/180)); c <- mean(cos(deg*pi/180))
  c(dir = (atan2(s, c)*180/pi) %% 360, R = sqrt(s^2 + c^2))
}
angle_entre <- function(a, b) { d <- abs((a - b) %% 360); min(d, 360 - d) }

# ================= (1) abans i despres del 2011, amb la resolucio de cada epoca
# tot aquest script treballa amb la mesura CRUA: es el que diagnostica el gir
A <- fread("derived/la_garriga_diari_ajuntament.csv")
A <- A[!is.na(ratxa_dir_bruta) & !is.na(ratxa_max)][, ratxa_dir := ratxa_dir_bruta]
A[, epoca := fifelse(any <= 2010, "2002-2010 (graus reals)",
             fifelse(any >= 2012, "2012-2023 (rumbs de 16)", "2011 (transicio)"))]
# nomes ratxes del semicercle nord i prou fortes: son les de Saligarda
nord <- A[(ratxa_dir >= 270 | ratxa_dir <= 90) & ratxa_max >= 30]
cat("=== (1) Direccio de la ratxa maxima diaria del sector nord (>= 30 km/h) ===\n")
print(nord[, {v <- dirmit(ratxa_dir)
              .(dies = .N, dir_mitjana = round(v[1], 1), R = round(v[2], 3),
                mediana = round(median(fifelse(ratxa_dir > 180, ratxa_dir - 360,
                                               ratxa_dir)) %% 360, 1))},
           by = epoca][order(epoca)])

cat("\n  distribucio per octants (% dels dies del sector nord):\n")
nord[, oct := cut((ratxa_dir + 22.5) %% 360, breaks = seq(0, 360, 45),
                  labels = c("N","NE","E","SE","S","SO","O","NO"), include.lowest = TRUE)]
print(dcast(nord[, .N, by = .(epoca, oct)][, frac := round(100*N/sum(N)), by = epoca],
            oct ~ epoca, value.var = "frac"))

cat("\n  any a any (nomes sector nord, >= 30 km/h):\n")
print(A[(ratxa_dir >= 270 | ratxa_dir <= 90) & ratxa_max >= 30,
        .(dies = .N, dir = round(dirmit(ratxa_dir)[1], 1)), by = any][order(any)])

# ================= (2) el flux de dia i el de nit son antiparal.lels?
g <- fread("derived/garriga_treball.csv")
g[, hi_dir_deg := hi_dir_deg_bruta]      # tambe en brut, per comparar epoques
g[, est := factor(est, levels = c("DJF","MAM","JJA","SON"))]
sel <- g[!is.na(hi_dir_deg) & !is.na(wind_mean) & wind_mean >= 5]
cat("\n=== (2) Eix del flux: nit contra tarda ===\n")
res <- sel[, {
  nit <- dirmit(hi_dir_deg[h >= 1 & h < 8])
  tar <- dirmit(hi_dir_deg[h >= 12 & h < 17])
  .(n_nit = sum(h >= 1 & h < 8), nit = round(nit[1], 1), R_nit = round(nit[2], 2),
    n_tarda = sum(h >= 12 & h < 17), tarda = round(tar[1], 1), R_tarda = round(tar[2], 2),
    angle = round(angle_entre(nit[1], tar[1]), 1))
}, by = est][order(est)]
print(res)
cat("  'angle' hauria de ser proper a 180 graus en una vall ben canalitzada.\n")
cat("  L'eix bisector implicit (mitjana dels dos sentits oposats):\n")
res[, bisec := round(((nit + ((tarda + 180) %% 360)) / 2) %% 360, 1)]
print(res[, .(est, nit, tarda, angle, eix_implicit = bisec)])

cat("\n=== Rumb geografic de la vall des de la Garriga: 338-350 graus ===\n")
cat(sprintf("  desfasament de l'eix nocturn observat respecte del rumb de la vall (344): %+.0f graus\n",
            ((dirmit(sel[h >= 1 & h < 8, hi_dir_deg])[1] - 344 + 180) %% 360) - 180))

# ------------------------------------------------------------------- figura
if (!dir.exists("figures")) dir.create("figures")
h <- nord[, .N, by = .(epoca, bin = cut(ratxa_dir %% 360, seq(-11.25, 371.25, 22.5)))]
nord[, dir_c := fifelse(ratxa_dir > 180, ratxa_dir - 360, ratxa_dir)]
p <- ggplot(nord, aes(dir_c, fill = epoca)) +
  geom_histogram(binwidth = 10, position = "identity", alpha = 0.55, colour = NA) +
  geom_vline(xintercept = -16, linetype = 2, colour = "#B2182B") +
  annotate("text", x = -16, y = Inf, label = " rumb de la vall (344 graus)",
           hjust = 0, vjust = 1.6, size = 3, colour = "#B2182B") +
  scale_x_continuous("Direccio de la ratxa maxima diaria (graus; negatiu = a l'oest del nord)",
                     breaks = seq(-90, 90, 30)) +
  scale_fill_manual(NULL, values = c("2002-2010 (graus reals)" = "#2166AC",
                                     "2011 (transicio)" = "grey60",
                                     "2012-2023 (rumbs de 16)" = "#B2182B")) +
  labs(y = "dies",
       title = "La direccio del vent del nord canvia de cop el 2011",
       subtitle = "Ratxes >= 30 km/h del semicercle nord. Abans anotades en graus reals; despres, en rumbs de 16.") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave("figures/F23_offset_penella.png", p, width = 9, height = 4.6, dpi = 150)
cat("\n-> figures/F23_offset_penella.png\n")
