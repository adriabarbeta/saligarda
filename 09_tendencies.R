# ==============================================================================
# 09_tendencies.R -- S'esta intensificant o afeblint la Saligarda?
#   (1) serie de 5 min, 2012-2023 (12 anys complets)
#   (2) serie diaria de l'Ajuntament, 2002-2023 (22 anys), amb la direccio de la
#       ratxa maxima diaria com a indicador; es valida contra (1) al periode comu
#   Tests: Mann-Kendall i pendent de Sen amb interval de confianca.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2); library(Kendall)})

# ------------------------------------------------------------- utilitats
sen <- function(y, x = seq_along(y)) {
  ok <- !is.na(y) & !is.na(x); y <- y[ok]; x <- x[ok]; n <- length(y)
  if (n < 5) return(c(pendent = NA, lo = NA, hi = NA))
  cmb <- combn(n, 2)
  s <- (y[cmb[2,]] - y[cmb[1,]]) / (x[cmb[2,]] - x[cmb[1,]])
  s <- s[is.finite(s)]
  N <- length(s)
  varS <- n * (n - 1) * (2 * n + 5) / 18
  C <- qnorm(0.975) * sqrt(varS)
  lo <- max(1, floor((N - C) / 2)); hi <- min(N, ceiling((N + C) / 2) + 1)
  ss <- sort(s)
  c(pendent = median(s), lo = ss[lo], hi = ss[hi])
}
mk <- function(y) { r <- MannKendall(y); c(tau = as.numeric(r$tau), p = as.numeric(r$sl)) }
informa <- function(nom, y, x, unitat) {
  s <- sen(y, x); m <- mk(y)
  cat(sprintf("  %-34s %+7.3f %s/any  IC95%% [%+.3f, %+.3f]  tau = %+.2f  p = %s\n",
              nom, s[1], unitat, s[2], s[3], m[1], format.pval(m[2], digits = 2)))
  invisible(c(s, m))
}

# ======================================================= (1) serie de 5 min
D <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]
D[, any_hiv := any + fifelse(mes == 12, 1L, 0L)]     # DJF: desembre va amb l'any seguent
ANYS <- 2012:2023                                     # anys complets

anual <- D[any %in% ANYS, .(
  dies_dades = .N,
  dies_sal   = sum(saligarda),
  frac_sal   = mean(saligarda),
  index_mitja= mean(u_dv, na.rm = TRUE),
  index_epis = mean(u_dv[saligarda], na.rm = TRUE),
  ratxa_epis = mean(ratxa_max[saligarda], na.rm = TRUE),
  p95_index  = quantile(u_dv, 0.95, na.rm = TRUE),
  durada     = mean(final[saligarda] - inici[saligarda], na.rm = TRUE),
  final_h    = mean(final[saligarda], na.rm = TRUE),
  # sensacio de fred
  wc_epis    = mean(wc_min[saligarda], na.rm = TRUE),
  gap_epis   = mean(wc_gap[saligarda], na.rm = TRUE),
  dies_wc_neg= sum(wc_min < 0, na.rm = TRUE),
  dies_gelada_nomes_vent = sum(wc_min < 0 & t_min >= 0, na.rm = TRUE)
), by = any][order(any)]
# 2012 nomes te 152 dies amb recorregut de vent: no es un any complet
anual <- anual[dies_dades >= 300]
cat("=== Serie de 5 min: resum anual (anys complets) ===\n")
print(anual[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

cat(sprintf("\n=== Tendencies, tot l'any (%d-%d, n = %d anys complets) ===\n",
            min(anual$any), max(anual$any), nrow(anual)))
informa("dies de Saligarda", anual$dies_sal, anual$any, "dies")
informa("index mitja de tots els dies", anual$index_mitja, anual$any, "km/h")
informa("intensitat mitjana dels episodis", anual$index_epis, anual$any, "km/h")
informa("ratxa maxima mitjana dels episodis", anual$ratxa_epis, anual$any, "km/h")
informa("percentil 95 de l'index", anual$p95_index, anual$any, "km/h")
informa("durada dels episodis", anual$durada, anual$any, "h")
informa("hora de finalitzacio", anual$final_h, anual$any, "h")
cat("\n  sensacio de fred:\n")
informa("sensacio minima en episodis", anual$wc_epis, anual$any, "C")
informa("rebaixa per vent en episodis", anual$gap_epis, anual$any, "C")
informa("dies amb sensacio < 0 C", anual$dies_wc_neg, anual$any, "dies")
informa("dies de gelada nomes per vent", anual$dies_gelada_nomes_vent, anual$any, "dies")

cat("\n=== Tendencies per estacio de l'any ===\n")
for (e in c("DJF","MAM","JJA","SON")) {
  sub <- if (e == "DJF") D[est == "DJF" & any_hiv %in% (min(ANYS)+1):max(ANYS)]
         else D[est == e & any %in% ANYS]
  sub[, grup := if (e == "DJF") any_hiv else any]
  a <- sub[, .(dies = sum(saligarda), idx = mean(u_dv, na.rm=TRUE),
               ie = mean(u_dv[saligarda], na.rm=TRUE), n = .N), by = grup][order(grup)]
  a <- a[n >= 60]
  cat(sprintf(" %s (n = %d anys)\n", e, nrow(a)))
  informa("   dies de Saligarda", a$dies, a$grup, "dies")
  informa("   index mitja", a$idx, a$grup, "km/h")
}

# ================================== (2) serie de l'Ajuntament: validacio
A <- fread("derived/la_garriga_diari_ajuntament.csv"); A[, date := as.IDate(date)]
# indicador: ratxa maxima diaria del sector nord i prou forta
SECTOR <- function(d) !is.na(d) & (d >= 315 | d <= 67.5)
A[, prox := SECTOR(ratxa_dir) & ratxa_max >= 30]

V <- merge(A[, .(date, prox, ratxa_max, ratxa_dir)],
           D[, .(date, saligarda, u_dv)], by = "date")
cat(sprintf("\n=== Validacio de l'indicador diari (periode comu, n = %d dies) ===\n", nrow(V)))
tc <- table(indicador = V$prox, index_5min = V$saligarda)
print(tc)
POD <- tc["TRUE","TRUE"] / sum(tc[,"TRUE"]); FAR <- tc["TRUE","FALSE"] / sum(tc["TRUE",])
cat(sprintf("  deteccio (POD) = %.2f | falses alarmes (FAR) = %.2f | encerts = %.2f\n",
            POD, FAR, sum(diag(tc))/sum(tc)))
cat(sprintf("  correlacio ratxa maxima diaria - index u_dv: r = %.2f\n",
            cor(V$ratxa_max, V$u_dv, use = "complete.obs")))

anualA <- A[any %in% 2002:2023 & !is.na(ratxa_dir),
            .(dies_dades = .N, dies_prox = sum(prox, na.rm = TRUE),
              ratxa_mitjana = mean(ratxa_max, na.rm = TRUE),
              frac_nord = mean(SECTOR(ratxa_dir)),
              wc_min_mitja = mean(wchill_min, na.rm = TRUE),
              dies_wc_neg = sum(wchill_min < 0, na.rm = TRUE),
              dies_gelada_nomes_vent = sum(wchill_min < 0 & tmin >= 0, na.rm = TRUE)),
            by = any][order(any)]
anualA <- anualA[dies_dades >= 330]
cor_anual <- merge(anual[, .(any, dies_sal)], anualA[, .(any, dies_prox)], by = "any")
cat(sprintf("  correlacio dels recomptes anuals (2012-2023): r = %.2f\n",
            cor(cor_anual$dies_sal, cor_anual$dies_prox)))

cat("\n", strrep("!", 74), "\n", sep = "")
cat("ATENCIO: 09b_homogeneitat.R detecta una RUPTURA ARTIFICIAL el 2011 a la\n")
cat("serie de vent (test de Pettitt p = 0,004; salt de +4,4 km/h a la ratxa\n")
cat("maxima diaria mitjana, de 27,8 a 32,2 km/h, i la fraccio de dies amb\n")
cat("ratxa >= 30 km/h passa de 0,25 a 0,49). Coincideix amb l'inici del\n")
cat("registre de 5 min (marc de 2011) i apunta a un canvi d'anemometre o\n")
cat("d'exposicio. Les tendencies 2002-2023 de VENT NO son interpretables:\n")
cat("es donen nomes per documentar l'artefacte.\n")
cat(strrep("!", 74), "\n")

cat(sprintf("\n=== [NO INTERPRETABLE] 2002-2023 amb l'indicador diari (n = %d anys) ===\n",
            nrow(anualA)))
informa("dies amb ratxa forta del nord", anualA$dies_prox, anualA$any, "dies")
informa("ratxa maxima diaria mitjana", anualA$ratxa_mitjana, anualA$any, "km/h")
informa("fraccio de dies amb ratxa del nord", anualA$frac_nord * 100, anualA$any, "%")
informa("sensacio de fred minima diaria", anualA$wc_min_mitja, anualA$any, "C")
informa("dies amb sensacio < 0 C", anualA$dies_wc_neg, anualA$any, "dies")
informa("dies de gelada nomes per vent", anualA$dies_gelada_nomes_vent, anualA$any, "dies")

# hivern
hivA <- A[mes %in% c(12,1,2) & !is.na(ratxa_dir)]
hivA[, any_hiv := any + fifelse(mes == 12, 1L, 0L)]
hA <- hivA[, .(dies = sum(prox, na.rm=TRUE), n = .N,
               ratxa = mean(ratxa_max, na.rm=TRUE),
               wc = mean(wchill_min, na.rm=TRUE),
               dies_wc_neg = sum(wchill_min < 0, na.rm=TRUE)),
           by = any_hiv][n >= 80][order(any_hiv)]
cat(sprintf("\n=== Nomes hivern (DJF), 2003-2024 (n = %d) ===\n", nrow(hA)))
informa("dies amb ratxa forta del nord", hA$dies, hA$any_hiv, "dies")
informa("ratxa maxima diaria mitjana", hA$ratxa, hA$any_hiv, "km/h")
informa("sensacio de fred minima diaria", hA$wc, hA$any_hiv, "C")
informa("dies amb sensacio < 0 C", hA$dies_wc_neg, hA$any_hiv, "dies")

# el mateix, restringit al tram homogeni posterior a la ruptura
cat("\n=== Nomes tram homogeni (2012-2023) de la serie de l'Ajuntament ===\n")
aH <- anualA[any >= 2012]
informa("dies amb ratxa forta del nord", aH$dies_prox, aH$any, "dies")
informa("ratxa maxima diaria mitjana", aH$ratxa_mitjana, aH$any, "km/h")
informa("sensacio de fred minima diaria", aH$wc_min_mitja, aH$any, "C")
cat("  -> coincideix amb la serie de 5 min: cap tendencia significativa\n")

# ==================================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"))

# La serie diaria te una ruptura el 2011: s'ajusten dos trams per separat i no
# s'hi dibuixa cap recta global, que seria enganyosa.
s1 <- sen(anual$dies_sal, anual$any)
sA <- sen(anualA[any <= 2011, dies_prox], anualA[any <= 2011, any])
sB <- sen(anualA[any >= 2012, dies_prox], anualA[any >= 2012, any])
d1 <- data.table(any = anual$any, valor = anual$dies_sal, tram = "a",
                 serie = "A. Index de 5 min, serie homogenia (u_dv >= 8 km/h)")
d2 <- data.table(any = anualA$any, valor = anualA$dies_prox,
                 tram = fifelse(anualA$any <= 2011, "fins 2011", "des de 2012"),
                 serie = "B. Indicador diari, amb ruptura el 2011 (ratxa >= 30 km/h del nord)")
p11 <- ggplot(rbind(d1, d2), aes(any, valor)) +
  geom_vline(data = data.table(serie = unique(d2$serie), x = 2011.5),
             aes(xintercept = x), linetype = 2, colour = "grey35") +
  geom_line(colour = "grey55") + geom_point(size = 1.6) +
  geom_smooth(aes(group = tram), method = "lm", formula = y ~ x, se = TRUE,
              colour = "#B2182B", fill = "#B2182B", alpha = 0.12, linewidth = 0.7) +
  facet_wrap(~serie, scales = "free", nrow = 1) +
  scale_x_continuous(NULL, breaks = seq(2002, 2024, 4)) +
  labs(y = "dies per any",
       title = "Frequencia anual de la Saligarda",
       subtitle = sprintf(paste0("A: %+.2f dies/any [%+.2f, %+.2f], no significativa.  ",
                                 "B: ruptura instrumental el 2011 (Pettitt p = 0,004); ",
                                 "per trams, %+.2f i %+.2f dies/any."),
                          s1[1], s1[2], s1[3], sA[1], sB[1])) +
  tema
ggsave("figures/F11_tendencia_frequencia.png", p11, width = 11, height = 4.2, dpi = 150)

mens <- D[any %in% ANYS, .(frac = mean(saligarda), idx = mean(u_dv, na.rm=TRUE)),
          by = .(any, mes)]
p12 <- ggplot(mens, aes(factor(mes), idx)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.5, linewidth = 0.3) +
  labs(x = "Mes", y = "Index mitja mensual (km/h)",
       title = "Cicle anual de la intensitat de la Saligarda",
       subtitle = "Component mitjana vall avall entre 00 i 10 UTC, per mes i any (2012-2023)") +
  tema
ggsave("figures/F12_cicle_anual.png", p12, width = 7.5, height = 4, dpi = 150)

w1 <- data.table(any = anual$any, valor = anual$wc_epis,
                 serie = "sensacio minima en episodis (5 min)")
w2 <- data.table(any = anualA$any, valor = anualA$wc_min_mitja,
                 serie = "sensacio minima diaria mitjana (Ajuntament)")
sw1 <- sen(anual$wc_epis, anual$any); sw2 <- sen(anualA$wc_min_mitja, anualA$any)
p13 <- ggplot(rbind(w1, w2), aes(any, valor)) +
  geom_line(colour = "grey55") + geom_point(size = 1.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#2166AC",
              fill = "#2166AC", alpha = 0.12, linewidth = 0.7) +
  facet_wrap(~serie, scales = "free", nrow = 1) +
  scale_x_continuous(NULL, breaks = seq(2002, 2024, 4)) +
  labs(y = "sensacio de fred (ºC)",
       title = "Tendencia de la sensacio de fred a la Garriga",
       subtitle = sprintf("Pendent de Sen: %+.3f C/any [%+.3f, %+.3f] (episodis) i %+.3f C/any [%+.3f, %+.3f] (diari)",
                          sw1[1], sw1[2], sw1[3], sw2[1], sw2[2], sw2[3])) +
  tema
ggsave("figures/F13_tendencia_sensacio_fred.png", p13, width = 10, height = 4, dpi = 150)

fwrite(anual, "derived/tendencia_anual_5min.csv")
fwrite(anualA, "derived/tendencia_anual_ajuntament.csv")
cat("\n-> figures/F11_tendencia_frequencia.png, figures/F12_cicle_anual.png\n")
