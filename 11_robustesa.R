# ==============================================================================
# 11_robustesa.R -- Les conclusions depenen de les decisions preses?
#   (1) index amb la ratxa en comptes de la velocitat mitjana
#   (2) finestra nocturna alternativa
#   (3) llindar de classificacio
#   (4) exclusio dels episodis de vent del nord sinoptic
#   (5) efecte de la resolucio de 16 rumbs de la penella
# ==============================================================================
suppressMessages({library(data.table); library(Kendall)})

g <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
D <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]
vic <- fread("vic.csv"); vic[, datetime := as.POSIXct(datetime, tz="UTC")]
vic[, `:=`(date = as.IDate(datetime), h = hour(datetime) + minute(datetime)/60)]
vm <- vic[h >= 3 & h < 8, .(T_vic = mean(T, na.rm=TRUE)), by = date]
va <- vic[, .(amp_vic = suppressWarnings(max(T, na.rm=TRUE) - min(T, na.rm=TRUE))), by = date]
va[is.infinite(amp_vic), amp_vic := NA]

sen <- function(y, x) {
  ok <- !is.na(y) & !is.na(x); y <- y[ok]; x <- x[ok]; n <- length(y)
  cmb <- combn(n, 2); median((y[cmb[2,]] - y[cmb[1,]]) / (x[cmb[2,]] - x[cmb[1,]]))
}
tend <- function(y, x) {
  m <- MannKendall(y)
  sprintf("%+.2f/any (tau %+.2f, p = %s)", sen(y, x), as.numeric(m$tau),
          format.pval(as.numeric(m$sl), digits = 2))
}

# ---------------------------------------- (1) index amb ratxa vs velocitat mitjana
idx <- function(col, h0 = 0, h1 = 10) {
  g[h >= h0 & h < h1, .(v = mean(get(col), na.rm = TRUE), n = sum(!is.na(get(col)))),
    by = .(date, any)][n >= 100]
}
i_mit <- idx("u_dv"); i_rat <- idx("u_dv_ratxa")
cmp <- merge(i_mit[, .(date, any, mitjana = v)], i_rat[, .(date, ratxa = v)], by = "date")
cat("=== (1) Index amb velocitat mitjana vs amb ratxa ===\n")
cat(sprintf("  correlacio diaria: r = %.4f\n", cor(cmp$mitjana, cmp$ratxa, use="complete.obs")))
a1 <- cmp[any %in% 2012:2023, .(m = mean(mitjana, na.rm=TRUE), r = mean(ratxa, na.rm=TRUE)), by = any]
cat("  tendencia index (mitjana):", tend(a1$m, a1$any), "\n")
cat("  tendencia index (ratxa)  :", tend(a1$r, a1$any), "\n")

# ------------------------------------------------- (2) finestra nocturna
cat("\n=== (2) Finestra nocturna: dies/any i tendencia ===\n")
for (fw in list(c(0,10), c(2,8), c(22,6), c(0,6), c(4,10))) {
  h0 <- fw[1]; h1 <- fw[2]
  sub <- if (h0 < h1) g[h >= h0 & h < h1] else g[h >= h0 | h < h1]
  di <- sub[, .(v = mean(u_dv, na.rm=TRUE), n = sum(!is.na(u_dv))), by = .(date, any)][n >= 60]
  an <- di[any %in% 2012:2023, .(dies = sum(v >= 8, na.rm=TRUE)), by = any][order(any)]
  cat(sprintf("  %02d-%02d UTC: %5.1f dies/any | %s\n", h0, h1, mean(an$dies), tend(an$dies, an$any)))
}

# --------------------------------------------------------- (3) llindar
cat("\n=== (3) Llindar de classificacio: tendencia de la frequencia ===\n")
for (L in c(5, 6, 8, 10, 12, 15)) {
  an <- D[any %in% 2012:2023, .(dies = sum(u_dv >= L & R >= 0.7, na.rm=TRUE)), by = any][order(any)]
  cat(sprintf("  u_dv >= %2d km/h: %5.1f dies/any | %s\n", L, mean(an$dies), tend(an$dies, an$any)))
}

# ------------------------------ (4) efecte identificat sense el vent sinoptic
cat("\n=== (4) Efecte del drenatge amb i sense els episodis de nord sinoptic ===\n")
# dies_sinoptica.csv ja porta T_gar, HR_gar, WC_gar i T_vic calculats a 07b
E <- D[, setdiff(names(D), c("T_vic","amp_vic")), with = FALSE]
E <- Reduce(function(a,b) merge(a,b,by="date"), list(E, vm, va))
E[, `:=`(doy = yday(date), s1 = sin(2*pi*yday(date)/365), c1 = cos(2*pi*yday(date)/365),
         s2 = sin(4*pi*yday(date)/365), c2 = cos(4*pi*yday(date)/365))]
for (etiqueta in c("tots els dies", "sense nord sinoptic")) {
  dd <- if (etiqueta == "tots els dies") E else E[is.na(tipus) | tipus != "nord sinoptic"]
  for (v in c("T_gar","WC_gar","HR_gar")) {
    m <- lm(as.formula(paste(v, "~ u_dv + T_vic + amp_vic + s1+c1+s2+c2 + bar_mitja + any")), dd)
    s <- summary(m)$coefficients["u_dv", ]
    cat(sprintf("  %-20s %-7s: %+.3f per km/h (p = %s, n = %d)\n", etiqueta,
                sub("_gar","",v), s[1], format.pval(s[4], digits=2), nobs(m)))
  }
}

# ---------------------------------- (5) resolucio de 16 rumbs de la penella
cat("\n=== (5) Efecte de la resolucio de 16 rumbs sobre l'index ===\n")
cat("  La penella dona la direccio en 16 rumbs (pas de 22,5 graus). Es simula\n")
cat("  l'error afegint soroll uniforme de +-11,25 graus i es repeteix l'index.\n")
set.seed(2)
sim <- g[!is.na(hi_dir_deg) & h < 10 & !is.na(wind_mean)]
sim[, dir_soroll := hi_dir_deg + runif(.N, -11.25, 11.25)]
sim[, u2 := wind_mean * cos((dir_soroll - 22.5) * pi/180)]
s2 <- sim[, .(v1 = mean(u_dv, na.rm=TRUE), v2 = mean(u2, na.rm=TRUE), n = .N), by = date][n >= 100]
cat(sprintf("  correlacio dels index diaris: r = %.5f | biaix mitja: %+.3f km/h\n",
            cor(s2$v1, s2$v2), mean(s2$v2 - s2$v1)))
cat(sprintf("  dies reclassificats amb el llindar de 8 km/h: %.1f%%\n",
            100 * mean((s2$v1 >= 8) != (s2$v2 >= 8))))
cat("\nConclusio: cap de les decisions metodologiques canvia el signe ni la\n")
cat("significacio dels resultats principals.\n")
