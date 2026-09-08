# ==============================================================================
# 10b_sinoptica_reanalisi.R -- Situacio sinoptica i reconstruccio de 45 anys
#   1) Mapes compostos de pressio: dies amb i sense Saligarda, i anomalia
#   2) Classificacio de tipus de circulacio (PCA en mode S + kmeans, que es el
#      metode de synoptReg, retirat del CRAN el 2023) i propensio per tipus
#   3) Reconstruccio estadistica de l'index de Saligarda 1980-2024 a partir de
#      la circulacio, amb validacio creuada any a any. Permet respondre la
#      pregunta de la tendencia amb 45 anys en comptes d'11.
#   Requereix haver executat 10a_baixa_ncep.R.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2); library(sf)
                  library(rnaturalearth); library(Kendall)})

sen <- function(y, x = seq_along(y)) {
  ok <- !is.na(y) & !is.na(x); y <- y[ok]; x <- x[ok]; n <- length(y)
  if (n < 5) return(c(pendent = NA, lo = NA, hi = NA))
  cmb <- combn(n, 2); s <- (y[cmb[2,]] - y[cmb[1,]]) / (x[cmb[2,]] - x[cmb[1,]])
  s <- s[is.finite(s)]; N <- length(s)
  C <- qnorm(0.975) * sqrt(n * (n - 1) * (2 * n + 5) / 18)
  ss <- sort(s)
  c(pendent = median(s), lo = ss[max(1, floor((N - C)/2))],
    hi = ss[min(N, ceiling((N + C)/2) + 1)])
}
informa <- function(nom, y, x, u) {
  s <- sen(y, x); m <- MannKendall(y)
  cat(sprintf("  %-36s %+7.4f %s/any  IC95%% [%+.4f, %+.4f]  tau = %+.2f  p = %s\n",
              nom, s[1], u, s[2], s[3], as.numeric(m$tau),
              format.pval(as.numeric(m$sl), digits = 2)))
}

# --------------------------------------------- 1. camps de reanalisi a matriu
a_matriu <- function(fitxer, hores = c("00", "06")) {
  R <- readRDS(fitxer)
  M <- rbindlist(lapply(R$dades, function(x) {          # x: [lat, lon, temps]
    nt <- dim(x)[3]
    data.table(ts = dimnames(x)[[3]],
               matrix(aperm(x, c(3, 1, 2)), nrow = nt))  # lat varia primer
  }))
  M[, `:=`(date = as.IDate(substr(ts, 1, 10), format = "%Y_%m_%d"),
           h = substr(ts, 12, 13))]
  M <- M[h %in% hores]
  cel <- grep("^V", names(M), value = TRUE)
  D <- M[, lapply(.SD, mean, na.rm = TRUE), by = date, .SDcols = cel]
  list(dates = D$date, X = as.matrix(D[, ..cel]),
       graella = as.data.table(expand.grid(lat = R$lat, lon = R$lon)))
}

cat("=== Carregant reanalisi NCEP/NCAR ===\n")
SLP <- a_matriu("derived/ncep_slp.rds")
SLP$X <- SLP$X / 100                                    # Pa -> hPa
cat(sprintf("  %d dies x %d punts de graella | %s -> %s\n", nrow(SLP$X), ncol(SLP$X),
            format(min(SLP$dates)), format(max(SLP$dates))))

# ---------------------------------------------- anomalies (fora el cicle anual)
anomalia <- function(X, dates) {
  doy <- data.table::yday(dates)
  clim <- matrix(NA_real_, 366, ncol(X))
  for (d in 1:366) {
    sel <- abs(((doy - d + 182) %% 365) - 182) <= 7     # finestra de +-7 dies
    if (sum(sel) > 10) clim[d, ] <- colMeans(X[sel, , drop = FALSE])
  }
  X - clim[doy, ]
}
A_slp <- anomalia(SLP$X, SLP$dates)

# ------------------------------------ 2. tipus de circulacio (PCA mode S + kmeans)
set.seed(1)
pca  <- prcomp(A_slp, center = TRUE, scale. = FALSE)
vexp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
nPC  <- which(vexp >= 0.90)[1]
cat(sprintf("\n=== Classificacio sinoptica: %d components (%.0f%% de la variancia) ===\n",
            nPC, 100 * vexp[nPC]))
SC <- pca$x[, 1:nPC, drop = FALSE]
K  <- 9
km <- kmeans(scale(SC), centers = K, nstart = 50, iter.max = 100)

clas <- data.table(date = SLP$dates, ct = km$cluster)   # ct = tipus de circulacio
dia  <- fread("derived/dies_sinoptica.csv"); dia[, date := as.IDate(date)]
obs_ct <- merge(clas, dia[, .(date, saligarda, u_dv)], by = "date")

frq <- clas[, .(dies_totals = .N), by = ct]
res_ct <- merge(obs_ct[, .(dies_obs = .N, frac_saligarda = mean(saligarda),
                           u_dv = mean(u_dv, na.rm = TRUE)), by = ct],
                frq, by = "ct")
res_ct[, frac_dies := dies_totals / nrow(clas)]
setorder(res_ct, -frac_saligarda)
cat("\n  tipus ordenats per propensio a la Saligarda:\n")
print(res_ct[, .(ct, dies_obs, frac_saligarda = round(frac_saligarda, 2),
                 u_dv = round(u_dv, 1), frac_dies = round(frac_dies, 3))])

favorables <- res_ct[frac_saligarda >= 0.60, ct]
cat("  tipus favorables (>= 60% de dies amb Saligarda):",
    if (length(favorables)) paste(favorables, collapse = ", ") else "cap", "\n")

# -------------------------- 3. reconstruccio 1980-2024 amb validacio creuada
dd <- cbind(data.table(date = SLP$dates), as.data.table(SC))
dd <- merge(dd, dia[, .(date, saligarda, u_dv)], by = "date", all.x = TRUE)
dd <- merge(dd, clas, by = "date")
dd[, `:=`(any = year(date), mes = month(date), doy = yday(date))]
dd[, `:=`(s1 = sin(2*pi*doy/365), c1 = cos(2*pi*doy/365),
          s2 = sin(4*pi*doy/365), c2 = cos(4*pi*doy/365))]
pcs    <- paste0("PC", 1:nPC)
form_u <- as.formula(paste("u_dv ~ s1 + c1 + s2 + c2 +", paste(pcs, collapse = " + ")))
form_s <- as.formula(paste("saligarda ~ s1 + c1 + s2 + c2 +", paste(pcs, collapse = " + ")))

obs <- dd[!is.na(u_dv)]
cat(sprintf("\n=== Reconstruccio: ajust sobre %d dies observats (%d-%d) ===\n",
            nrow(obs), min(obs$any), max(obs$any)))
cv <- rbindlist(lapply(sort(unique(obs$any)), function(a) {
  m <- lm(form_u, data = obs[any != a])
  data.table(any = a, obs = obs[any == a, u_dv], pred = predict(m, obs[any == a]))
}))
cat(sprintf("  validacio creuada any a any: r = %.3f | R2 = %.3f | RMSE = %.2f km/h\n",
            cor(cv$obs, cv$pred), cor(cv$obs, cv$pred)^2,
            sqrt(mean((cv$obs - cv$pred)^2))))
cv_an <- cv[, .(obs = mean(obs), pred = mean(pred)), by = any]
cat(sprintf("  a escala anual: r = %.3f (n = %d anys)\n", cor(cv_an$obs, cv_an$pred),
            nrow(cv_an)))

m_u <- lm(form_u, data = obs)
m_s <- glm(form_s, data = obs, family = binomial())
dd[, `:=`(u_rec = predict(m_u, dd), p_sal = predict(m_s, dd, type = "response"))]

anual <- dd[, .(dies = .N, u_rec = mean(u_rec), dies_sal_esp = sum(p_sal),
                n_fav = sum(ct %in% favorables)), by = any][dies >= 350][order(any)]

cat("\n=== Tendencies sobre 45 anys de circulacio (1980-2024) ===\n")
informa("index de Saligarda reconstruit", anual$u_rec, anual$any, "km/h")
informa("dies de Saligarda esperats", anual$dies_sal_esp, anual$any, "dies")
if (length(favorables)) informa("dies amb circulacio favorable", anual$n_fav, anual$any, "dies")

hiv <- dd[mes %in% c(12, 1, 2)]
hiv[, any_hiv := any + fifelse(mes == 12, 1L, 0L)]
hA <- hiv[, .(n = .N, u_rec = mean(u_rec), dies_sal = sum(p_sal)),
          by = any_hiv][n >= 85][order(any_hiv)]
cat(sprintf("\n  nomes hivern (DJF), n = %d hiverns:\n", nrow(hA)))
informa("index reconstruit", hA$u_rec, hA$any_hiv, "km/h")
informa("dies de Saligarda esperats", hA$dies_sal, hA$any_hiv, "dies")

# ==================================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
costa <- suppressWarnings(ne_countries(scale = "medium", returnclass = "sf"))
XL <- range(SLP$graella$lon); YL <- range(SLP$graella$lat)
tema_mapa <- theme_bw(base_size = 9) +
  theme(panel.grid = element_line(colour = "grey92"),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")

camp <- function(v) cbind(copy(SLP$graella), valor = as.numeric(v))

# ------------------------------------------------------ vent geostrofic del camp
# f k x Vg = -(1/rho) grad p  =>  ug = -(1/(rho f)) dp/dy ; vg = (1/(rho f)) dp/dx
# Diferencies centrades sobre la graella; NA a les vores. Retorna m/s.
LAT <- sort(unique(SLP$graella$lat), decreasing = TRUE)
LON <- sort(unique(SLP$graella$lon))
geostrofic <- function(v, cada = 2, max_graus = 3.2, escala = NULL) {
  P <- matrix(as.numeric(v) * 100, nrow = length(LAT), ncol = length(LON))  # Pa
  rho <- 1.22; om <- 7.292e-5
  dlat_m <- abs(diff(LAT)[1]) * 111320
  ug <- vg <- matrix(NA_real_, nrow(P), ncol(P))
  for (i in 2:(nrow(P) - 1)) for (j in 2:(ncol(P) - 1)) {
    f <- 2 * om * sin(LAT[i] * pi / 180)
    dlon_m <- abs(diff(LON)[1]) * 111320 * cos(LAT[i] * pi / 180)
    dpdy <- (P[i - 1, j] - P[i + 1, j]) / (2 * dlat_m)   # la fila i-1 es al nord
    dpdx <- (P[i, j + 1] - P[i, j - 1]) / (2 * dlon_m)
    ug[i, j] <- -dpdy / (rho * f); vg[i, j] <- dpdx / (rho * f)
  }
  D <- data.table(lat = rep(LAT, times = length(LON)),
                  lon = rep(LON, each = length(LAT)),
                  u = as.numeric(ug), v = as.numeric(vg))
  D <- D[!is.na(u)]
  D <- D[lat %in% LAT[seq(2, length(LAT) - 1, by = cada)] &
         lon %in% LON[seq(2, length(LON) - 1, by = cada)]]
  # de m/s a graus, corregint la convergencia dels meridians. L'escala s'ajusta
  # sola perque la fletxa mes llarga ocupi 'max_graus' i no tapi el mapa.
  D[, `:=`(dx = u / cos(lat * pi / 180), dy = v)]
  if (is.null(escala)) escala <- max_graus / max(sqrt(D$dx^2 + D$dy^2))
  D[, `:=`(xend = lon + escala * dx, yend = lat + escala * dy,
           vel = sqrt(u^2 + v^2))]
  attr(D, "escala") <- escala
  D[]
}
FLETXA <- arrow(length = unit(0.09, "cm"), type = "closed", angle = 22)
COL_VENT <- "#08306B"
sel  <- merge(data.table(date = SLP$dates, i = seq_along(SLP$dates)),
              dia[est == "DJF", .(date, saligarda)], by = "date")
i_si <- sel[saligarda == TRUE, i]; i_no <- sel[saligarda == FALSE, i]
c_si <- camp(colMeans(SLP$X[i_si, ])); c_no <- camp(colMeans(SLP$X[i_no, ]))
c_di <- camp(colMeans(SLP$X[i_si, ]) - colMeans(SLP$X[i_no, ]))
cat(sprintf("\n  composits DJF: %d dies amb Saligarda, %d sense\n", length(i_si), length(i_no)))

MP <- rbind(cbind(c_si, panell = "dies amb Saligarda (DJF)"),
            cbind(c_no, panell = "dies sense Saligarda (DJF)"))
esc15 <- attr(geostrofic(c_si$valor), "escala")   # la mateixa als dos panells
V15 <- rbind(cbind(geostrofic(c_si$valor, escala = esc15), panell = "dies amb Saligarda (DJF)"),
             cbind(geostrofic(c_no$valor, escala = esc15), panell = "dies sense Saligarda (DJF)"))
p15 <- ggplot(MP, aes(lon, lat)) +
  geom_tile(aes(fill = valor)) +
  geom_contour(aes(z = valor), colour = "grey45", linewidth = 0.2, binwidth = 4) +
  geom_sf(data = costa, fill = NA, colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_segment(data = V15, aes(x = lon, y = lat, xend = xend, yend = yend),
               arrow = FLETXA, colour = COL_VENT, linewidth = 0.42, inherit.aes = FALSE) +
  facet_wrap(~panell) +
  scale_fill_distiller("hPa", palette = "RdYlBu") +
  scale_x_continuous(breaks = seq(-15, 15, 10)) +
  scale_y_continuous(breaks = seq(35, 50, 5)) +
  coord_sf(xlim = XL, ylim = YL, expand = FALSE) +
  labs(x = NULL, y = NULL,
       title = "Pressio al nivell del mar (NCEP/NCAR, mitjana 00-06 UTC)",
       subtitle = paste("Composits d'hivern segons hi hagi Saligarda a la Garriga o no.",
                        sprintf("Fletxes blaves: vent geostrofic (la mes llarga, %.0f km/h).",
                                3.6 * max(V15$vel)))) +
  tema_mapa
ggsave("figures/F15_composits_slp.png", p15, width = 9, height = 4.8, dpi = 150)

V15b <- geostrofic(c_di$valor)
p15b <- ggplot(c_di, aes(lon, lat)) +
  geom_tile(aes(fill = valor)) +
  geom_contour(aes(z = valor), colour = "grey45", linewidth = 0.2, binwidth = 2) +
  geom_sf(data = costa, fill = NA, colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_segment(data = V15b, aes(x = lon, y = lat, xend = xend, yend = yend),
               arrow = FLETXA, colour = COL_VENT, linewidth = 0.42, inherit.aes = FALSE) +
  scale_fill_gradient2("hPa", low = "#2166AC", mid = "white", high = "#B2182B") +
  scale_x_continuous(breaks = seq(-15, 15, 10)) +
  scale_y_continuous(breaks = seq(35, 50, 5)) +
  coord_sf(xlim = XL, ylim = YL, expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Anomalia de pressio (DJF)",
       subtitle = paste("Dies amb Saligarda menys dies sense.",
                        sprintf("
Fletxes blaves: vent geostrofic ANOMAL (la mes llarga, %.0f km/h).",
                                3.6 * max(V15b$vel)))) +
  tema_mapa
ggsave("figures/F15b_anomalia_slp.png", p15b, width = 5.4, height = 5, dpi = 150)

ordre <- res_ct$ct
cent <- rbindlist(lapply(ordre, function(k) {
  cbind(camp(colMeans(SLP$X[km$cluster == k, , drop = FALSE])), ct = k,
        etiqueta = sprintf("tipus %d - %.0f%% de dies amb Saligarda (%.0f%% dels dies)",
                           k, 100 * res_ct[ct == k, frac_saligarda],
                           100 * res_ct[ct == k, frac_dies]))
}))
cent[, etiqueta := factor(etiqueta, levels = unique(etiqueta))]
V16 <- rbindlist(lapply(ordre, function(k) {
  cbind(geostrofic(colMeans(SLP$X[km$cluster == k, , drop = FALSE]), cada = 3),
        etiqueta = cent[ct == k, etiqueta[1]])
}))
p16 <- ggplot(cent, aes(lon, lat)) +
  geom_tile(aes(fill = valor)) +
  geom_contour(aes(z = valor), colour = "grey45", linewidth = 0.18, binwidth = 5) +
  geom_sf(data = costa, fill = NA, colour = "black", linewidth = 0.2, inherit.aes = FALSE) +
  geom_segment(data = V16, aes(x = lon, y = lat, xend = xend, yend = yend),
               arrow = FLETXA, colour = COL_VENT, linewidth = 0.36, inherit.aes = FALSE) +
  facet_wrap(~etiqueta, ncol = 3) +
  scale_fill_distiller("hPa", palette = "RdYlBu") +
  scale_x_continuous(breaks = seq(-15, 15, 10)) +
  scale_y_continuous(breaks = seq(35, 50, 5)) +
  coord_sf(xlim = XL, ylim = YL, expand = FALSE) +
  labs(x = NULL, y = NULL, title = "Tipus de circulacio i propensio a la Saligarda",
       subtitle = paste("PCA en mode S sobre anomalies diaries de pressio + kmeans,",
                        "1980-2024. Fletxes blaves: vent geostrofic.")) +
  tema_mapa
ggsave("figures/F16_tipus_circulacio.png", p16, width = 9.5, height = 9.5, dpi = 150)

s1 <- sen(anual$dies_sal_esp, anual$any)
p17 <- ggplot(anual, aes(any, dies_sal_esp)) +
  geom_line(colour = "grey55") + geom_point(size = 1.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#B2182B",
              fill = "#B2182B", alpha = 0.12, linewidth = 0.7) +
  scale_x_continuous(NULL, breaks = seq(1980, 2025, 5)) +
  labs(y = "dies de Saligarda esperats",
       title = "Frequencia de la Saligarda reconstruida des de la circulacio, 1980-2024",
       subtitle = sprintf("Pendent de Sen: %+.2f dies/any [%+.2f, %+.2f]",
                          s1[1], s1[2], s1[3])) +
  theme_bw(base_size = 10)
ggsave("figures/F17_reconstruccio_45anys.png", p17, width = 8.5, height = 4.2, dpi = 150)

fwrite(anual, "derived/reconstruccio_anual.csv")
fwrite(clas,  "derived/tipus_circulacio.csv")
# components principals de la circulacio: entrada del model predictiu (12_)
fwrite(cbind(data.table(date = SLP$dates), as.data.table(SC)), "derived/pcs_slp.csv")
cat("\n-> figures F15, F15b, F16, F17 i derived/reconstruccio_anual.csv\n")
