# ==============================================================================
# 09b_homogeneitat.R -- Es fiable la tendencia de la serie de l'Ajuntament?
#   La serie de 5 min (2012-2023) no mostra cap tendencia en la frequencia ni
#   en la intensitat, pero la serie diaria de l'Ajuntament (2002-2023) dona
#   +4,8 dies/any i +0,32 km/h/any. Abans d'acceptar-ho cal comprovar:
#     (1) son la mateixa estacio? -> comparacio diaria al periode comu
#     (2) hi ha discontinuitats artificials? -> Pettitt i punts de ruptura
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2); library(strucchange)})

A <- fread("derived/la_garriga_diari_ajuntament.csv"); A[, date := as.IDate(date)]
g <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]

# resum diari de la Davis (dia civil complet)
davis <- g[, .(n = .N,
               tmax_d = max(temp_out, na.rm = TRUE),
               tmin_d = min(temp_out, na.rm = TRUE),
               ratxa_d = max(hi_speed, na.rm = TRUE),
               hrmax_d = max(out_hum, na.rm = TRUE),
               hrmin_d = min(out_hum, na.rm = TRUE)), by = date]
for (cn in c("tmax_d","tmin_d","ratxa_d","hrmax_d","hrmin_d"))
  set(davis, which(is.infinite(davis[[cn]])), cn, NA)
davis <- davis[n >= 250]

M <- merge(A, davis, by = "date")
cat(sprintf("=== (1) Mateixa estacio? Periode comu: %d dies ===\n", nrow(M)))
comp <- function(a, b, nom, u) {
  ok <- !is.na(a) & !is.na(b)
  cat(sprintf("  %-22s n=%5d  biaix=%+6.2f %s  EAM=%5.2f  r=%.4f\n",
              nom, sum(ok), mean(a[ok] - b[ok]), u, mean(abs(a[ok] - b[ok])),
              cor(a[ok], b[ok])))
}
comp(M$tmax, M$tmax_d, "T maxima", "C")
comp(M$tmin, M$tmin_d, "T minima", "C")
comp(M$hr_max, M$hrmax_d, "HR maxima", "%")
comp(M$hr_min, M$hrmin_d, "HR minima", "%")
comp(M$ratxa_max, M$ratxa_d, "ratxa maxima", "km/h")
cat("  (biaix ~0 i r ~1 => el full de l'Ajuntament es un resum de la mateixa Davis)\n")

cat("\n  biaix de la ratxa maxima per any (detecta canvis de criteri o d'aparell):\n")
print(M[!is.na(ratxa_max) & !is.na(ratxa_d),
        .(dies = .N, ajunt = round(mean(ratxa_max),1), davis = round(mean(ratxa_d),1),
          biaix = round(mean(ratxa_max - ratxa_d),2), r = round(cor(ratxa_max, ratxa_d),3)),
        by = any][order(any)])

# ------------------------------------------------- (2) discontinuitats
cat("\n=== (2) Discontinuitats a la serie diaria de l'Ajuntament ===\n")
an <- A[!is.na(ratxa_max), .(dies = .N, ratxa = mean(ratxa_max),
                             p95 = quantile(ratxa_max, .95),
                             frac30 = mean(ratxa_max >= 30)), by = any][dies >= 330][order(any)]
print(an[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

# test de Pettitt (canvi de posicio en un punt desconegut), implementat a ma
pettitt <- function(x) {
  n <- length(x); U <- numeric(n - 1)
  for (t in 1:(n - 1)) U[t] <- sum(outer(x[1:t], x[(t+1):n], function(a,b) sign(a-b)))
  K <- max(abs(U)); k <- which.max(abs(U))
  p <- 2 * exp(-6 * K^2 / (n^3 + n^2))
  list(punt = k, K = K, p = min(1, p))
}
pt <- pettitt(an$ratxa)
cat(sprintf("\n  Pettitt sobre la ratxa mitjana anual: ruptura a %d (K = %.0f, p = %.4f)\n",
            an$any[pt$punt], pt$K, pt$p))
cat(sprintf("  mitjana abans (%d-%d): %.1f km/h | despres (%d-%d): %.1f km/h | salt: %+.1f\n",
            min(an$any), an$any[pt$punt], mean(an$ratxa[1:pt$punt]),
            an$any[pt$punt + 1], max(an$any), mean(an$ratxa[(pt$punt+1):nrow(an)]),
            mean(an$ratxa[(pt$punt+1):nrow(an)]) - mean(an$ratxa[1:pt$punt])))

bp <- breakpoints(an$ratxa ~ 1, h = 4)
cat("\n  punts de ruptura (strucchange):",
    if (all(is.na(bp$breakpoints))) "cap" else paste(an$any[bp$breakpoints], collapse = ", "), "\n")

# el mateix, pero nomes al periode on tambe hi ha la Davis
an2 <- an[any >= 2012]
cat(sprintf("\n  tendencia de la ratxa mitjana anual nomes 2012-2023: %+.3f km/h/any\n",
            coef(lm(ratxa ~ any, an2))[2]))
cat(sprintf("  tendencia 2002-2011 (nomes Ajuntament):               %+.3f km/h/any\n",
            coef(lm(ratxa ~ any, an[any <= 2011]))[2]))

# --------------------- (3) la temperatura tambe te la ruptura del 2011?
cat("\n=== (3) Homogeneitat de la temperatura (sensor diferent de l'anemometre) ===\n")
anT <- A[!is.na(tmin) & !is.na(tmax),
         .(dies = .N, tmin = mean(tmin), tmax = mean(tmax),
           wc = mean(wchill_min, na.rm = TRUE)), by = any][dies >= 330][order(any)]
for (v in c("tmin","tmax","wc")) {
  pv <- pettitt(anT[[v]])
  cat(sprintf("  %-5s: ruptura a %d (p = %.3f) | tendencia 2002-2023 = %+.4f C/any | 2012-2023 = %+.4f\n",
              v, anT$any[pv$punt], pv$p,
              coef(lm(anT[[v]] ~ anT$any))[2],
              coef(lm(anT[[v]][anT$any >= 2012] ~ anT$any[anT$any >= 2012]))[2]))
}
cat("\n  Si la ruptura del 2011 NO surt a tmin/tmax, el sensor de temperatura es\n")
cat("  homogeni i la seva tendencia si que es interpretable; la de vent, no.\n")

# ------------------------------------------------------------------- figura
if (!dir.exists("figures")) dir.create("figures")
cmp <- M[!is.na(ratxa_max) & !is.na(ratxa_d),
         .(any, ajunt = ratxa_max, davis = ratxa_d)]
p14 <- ggplot(melt(rbind(
        data.table(any = an$any, valor = an$ratxa, serie = "Ajuntament (resum diari)"),
        M[!is.na(ratxa_d), .(valor = mean(ratxa_d)), by = .(any)][
          , .(any, valor, serie = "Davis (5 min)")]),
        id.vars = c("any","serie"))[, .(any, serie, value)],
      aes(any, value, colour = serie)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  geom_vline(xintercept = an$any[pt$punt] + 0.5, linetype = 2, colour = "grey40") +
  scale_colour_manual(NULL, values = c("Ajuntament (resum diari)" = "#B2182B",
                                       "Davis (5 min)" = "#2166AC")) +
  scale_x_continuous(NULL, breaks = seq(2002, 2024, 2)) +
  labs(y = "ratxa maxima diaria mitjana (km/h)",
       title = "Ratxa maxima diaria: full de l'Ajuntament contra registre de 5 min",
       subtitle = sprintf("Linia discontinua: ruptura detectada pel test de Pettitt (%d)",
                          an$any[pt$punt])) +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave("figures/F14_homogeneitat.png", p14, width = 8.5, height = 4.5, dpi = 150)
cat("\n-> figures/F14_homogeneitat.png\n")
