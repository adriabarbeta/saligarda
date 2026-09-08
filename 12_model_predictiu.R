# ==============================================================================
# 12_model_predictiu.R -- Pronostic diari de la Saligarda
#
#   OBJECTIU: a les 18 UTC del dia D-1, dir que fara la nit vinent.
#     y1 = u_dv        intensitat mitjana vall avall 00-10 UTC (km/h)
#     y2 = saligarda   u_dv >= 8 km/h i |R| >= 0,7
#     y3 = wc_min      sensacio de fred minima de la matinada (ºC)
#
#   HIGIENE TEMPORAL: cap predictor pot venir de despres de les 17 UTC del dia
#   D-1, perque el drenatge ja s'engega cap a les 18 UTC. Unica excepcio: les
#   components principals de la pressio del dia D, que en operatiu vindrien del
#   pronostic d'un model numeric. Es l'enfocament de "prognosi perfecta", habitual
#   en downscaling estadistic, i cal dir-ho: la destresa real seria una mica
#   inferior, tant com s'equivoqui el model numeric.
#
#   VALIDACIO: deixant un any sencer fora cada vegada. Mai dies solts: la
#   persistencia meteorologica inflaria la destresa.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2); library(ranger); library(pROC)})
set.seed(7)

# ============================================================ 1. PREDICTORS
g   <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
D   <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]
vic <- fread("vic.csv"); vic[, datetime := as.POSIXct(datetime, tz = "UTC")]
vic[, `:=`(date = as.IDate(datetime), h = hour(datetime) + minute(datetime) / 60)]
PC  <- fread("derived/pcs_slp.csv"); PC[, date := as.IDate(date)]

# --- estat del vespre a la Garriga (12-17 UTC del dia D-1) -------------------
vespre <- g[h >= 12 & h < 17, .(
  gar_T   = mean(temp_out, na.rm = TRUE),
  gar_HR  = mean(out_hum,  na.rm = TRUE),
  gar_P   = mean(bar,      na.rm = TRUE),
  gar_vent= mean(wind_mean,na.rm = TRUE),
  gar_udv = mean(u_dv,     na.rm = TRUE)), by = date]
# amplitud termica i pluja del dia D-1 fins a les 17 UTC (res posterior a l'hora
# d'emissio: el maxim diari cau cap a les 12-14 UTC i el minim a la sortida del sol)
diari_g <- g[h < 17, .(gar_amp = suppressWarnings(max(temp_out, na.rm = TRUE) -
                                                  min(temp_out, na.rm = TRUE)),
                       gar_pluja = sum(rain, na.rm = TRUE)), by = date]
diari_g[is.infinite(gar_amp), gar_amp := NA]

# --- estat del vespre i del dia a Vic ---------------------------------------
vespre_v <- vic[h >= 12 & h < 17, .(vic_T = mean(T, na.rm = TRUE),
                                    vic_HR = mean(HR, na.rm = TRUE)), by = date]
diari_v <- vic[h < 17, .(vic_amp = suppressWarnings(max(T, na.rm = TRUE) - min(T, na.rm = TRUE)),
                         vic_pluja = sum(PPT, na.rm = TRUE)), by = date]
diari_v[is.infinite(vic_amp), vic_amp := NA]

X <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE),
            list(vespre, diari_g, vespre_v, diari_v))
setorder(X, date)
X[, dP24 := gar_P - shift(gar_P)]                 # tendencia baromatrica 24 h
X[, contrast_vespre := vic_T - gar_T]             # Vic ja mes freda que la Garriga?
# tot aixo es del dia D-1: es desplaca a la nit seguent
X[, date := date + 1L]

# --- nit anterior (persistencia) --------------------------------------------
ant <- D[, .(date = date + 1L, udv_ahir = u_dv, sal_ahir = as.integer(saligarda))]

# --- muntatge ----------------------------------------------------------------
d <- Reduce(function(a, b) merge(a, b, by = "date"),
            list(D[, .(date, any, mes, est, u_dv, saligarda, wc_min, R)], X, ant,
                 PC[, .(date, PC1, PC2, PC3, PC4, PC5)]))
d[, `:=`(doy = yday(date),
         s1 = sin(2*pi*yday(date)/365), c1 = cos(2*pi*yday(date)/365),
         s2 = sin(4*pi*yday(date)/365), c2 = cos(4*pi*yday(date)/365))]
d[, sal := as.integer(saligarda)]
d[, sal_f := factor(sal, levels = c(0, 1))]
d <- d[complete.cases(d[, .(u_dv, gar_T, gar_HR, gar_P, gar_vent, gar_udv, gar_amp,
                            vic_T, vic_amp, dP24, contrast_vespre, udv_ahir,
                            PC1, PC2, PC3, PC4, PC5)])]
d <- d[any %in% 2013:2023]                        # anys complets i homogenis
cat(sprintf("=== Mostra: %d nits, %d-%d ===\n", nrow(d), min(d$any), max(d$any)))
cat(sprintf("    dies de Saligarda: %d (%.1f%%)\n", sum(d$sal), 100*mean(d$sal)))

ESTACIO  <- c("s1","c1","s2","c2")
SINOPTIC <- c("PC1","PC2","PC3","PC4","PC5")
LOCAL    <- c("gar_T","gar_HR","gar_P","gar_vent","gar_udv","gar_amp","gar_pluja",
              "vic_T","vic_HR","vic_amp","vic_pluja","dP24","contrast_vespre",
              "udv_ahir","sal_ahir")

# ================================================= 2. MODELS I VALIDACIO
anys <- sort(unique(d$any))
prediu <- function(vars, tipus = c("lm","rf"), objectiu = "u_dv") {
  tipus <- match.arg(tipus)
  f <- as.formula(paste(objectiu, "~", paste(vars, collapse = " + ")))
  rbindlist(lapply(anys, function(a) {
    tr <- d[any != a]; te <- d[any == a]
    if (tipus == "lm") {
      p <- if (objectiu == "sal")
             predict(glm(f, tr, family = binomial()), te, type = "response")
           else predict(lm(f, tr), te)
    } else {
      if (objectiu == "sal") {          # ranger vol un factor per a probabilitats
        f <- as.formula(paste("sal_f ~", paste(vars, collapse = " + ")))
        m <- ranger(f, tr, num.trees = 500, min.node.size = 5,
                    probability = TRUE, importance = "none")
        p <- predict(m, te)$predictions[, "1"]
      } else {
        m <- ranger(f, tr, num.trees = 500, min.node.size = 5, importance = "none")
        p <- predict(m, te)$predictions
      }
    }
    data.table(date = te$date, any = a, est = te$est,
               obs = te[[objectiu]], pred = as.numeric(p))
  }))
}
MSE_REF <- NULL          # error de la climatologia, per calcular la destresa
metr <- function(cv, nom, referencia = FALSE) {
  mse <- mean((cv$obs - cv$pred)^2)
  if (referencia) MSE_REF <<- mse
  r2 <- 1 - sum((cv$obs - cv$pred)^2) / sum((cv$obs - mean(cv$obs))^2)
  ss <- if (is.null(MSE_REF)) NA_real_ else 1 - mse / MSE_REF
  cat(sprintf("  %-42s R2 = %6.3f | RMSE = %4.2f | EAM = %4.2f | destresa vs clim. = %s\n",
              nom, r2, sqrt(mse), mean(abs(cv$obs - cv$pred)),
              if (is.na(ss)) "  -  " else sprintf("%+.3f", ss)))
  invisible(r2)
}
BRIER_REF <- NULL
metr_bin <- function(cv, nom, tall = 0.5) {
  a <- suppressMessages(auc(roc(cv$obs, cv$pred, quiet = TRUE)))
  br <- mean((cv$pred - cv$obs)^2)
  if (is.null(BRIER_REF)) BRIER_REF <<- br
  bss <- 1 - br / BRIER_REF
  p <- cv$pred >= tall; o <- cv$obs == 1
  POD <- sum(p & o) / sum(o); FAR <- sum(p & !o) / sum(p)
  PSS <- POD - sum(p & !o) / sum(!o)                    # Peirce
  cat(sprintf("  %-42s AUC = %.3f | Brier = %.3f | encerts = %.3f | POD = %.2f | FAR = %.2f | PSS = %.2f\n",
              nom, a, br, mean(p == o), POD, FAR, PSS))
  invisible(a)
}

cat("\n=== A) INTENSITAT (u_dv, km/h) - validacio deixant un any fora ===\n")
cat("  referencies:\n")
cli  <- prediu(ESTACIO, "lm");                 metr(cli,  "  climatologia (nomes cicle anual)", TRUE)
per  <- copy(d)[, .(obs = u_dv, pred = udv_ahir)]; metr(per, "  persistencia (la nit d'ahir)")
cat("  models:\n")
m_sin <- prediu(c(ESTACIO, SINOPTIC), "lm");   metr(m_sin, "  A1 sinoptic (prognosi perfecta)")
m_loc <- prediu(c(ESTACIO, LOCAL), "lm");      metr(m_loc, "  A2 local (observacions del vespre)")
m_tot <- prediu(c(ESTACIO, SINOPTIC, LOCAL), "lm"); metr(m_tot, "  A3 sinoptic + local")
m_rf  <- prediu(c(ESTACIO, SINOPTIC, LOCAL), "rf"); metr(m_rf,  "  A4 bosc aleatori (mateixes variables)")

cat("\n  destresa per estacio de l'any (model A4):\n")
print(m_rf[, .(nits = .N, R2 = round(1 - sum((obs-pred)^2)/sum((obs-mean(obs))^2), 3),
               RMSE = round(sqrt(mean((obs-pred)^2)), 2)), by = est][order(est)])

cat("\n=== B) OCORRENCIA (probabilitat de Saligarda) ===\n")
b_cli <- prediu(ESTACIO, "lm", "sal");                    metr_bin(b_cli, "  climatologia")
b_sin <- prediu(c(ESTACIO, SINOPTIC), "lm", "sal");       metr_bin(b_sin, "  B1 sinoptic")
b_loc <- prediu(c(ESTACIO, LOCAL), "lm", "sal");          metr_bin(b_loc, "  B2 local")
b_tot <- prediu(c(ESTACIO, SINOPTIC, LOCAL), "lm", "sal");metr_bin(b_tot, "  B3 sinoptic + local")
b_rf  <- prediu(c(ESTACIO, SINOPTIC, LOCAL), "rf", "sal");metr_bin(b_rf,  "  B4 bosc aleatori")

cat("\n=== C) SENSACIO DE FRED MINIMA (wc_min, ºC) ===\n")
MSE_REF <- NULL
w_cli <- prediu(ESTACIO, "lm", "wc_min");                 metr(w_cli, "  climatologia", TRUE)
w_rf  <- prediu(c(ESTACIO, SINOPTIC, LOCAL), "rf", "wc_min"); metr(w_rf, "  C1 bosc aleatori")

# ------------------------------------------------- importancia de variables
imp <- ranger(as.formula(paste("u_dv ~", paste(c(ESTACIO, SINOPTIC, LOCAL), collapse = " + "))),
              d, num.trees = 800, importance = "permutation")
IM <- data.table(variable = names(imp$variable.importance),
                 imp = as.numeric(imp$variable.importance))[order(-imp)]
IM[, rel := 100 * imp / max(imp)]
cat("\n=== Importancia de les variables (permutacio, u_dv) ===\n")
print(head(IM[, .(variable, importancia_relativa = round(rel, 1))], 12))

# ==================================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")

p19a <- ggplot(m_rf, aes(pred, obs)) +
  geom_abline(slope = 1, colour = "grey60", linetype = 2) +
  geom_point(alpha = 0.15, size = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#B2182B", linewidth = 0.7) +
  facet_wrap(~est, nrow = 1) +
  labs(x = "Pronostic (km/h)", y = "Observat (km/h)",
       title = "Pronostic de la intensitat de la Saligarda",
       subtitle = "Bosc aleatori, validacio deixant un any sencer fora (2013-2023)") + tema
ggsave("figures/F19_pronostic_intensitat.png", p19a, width = 9.5, height = 3.6, dpi = 150)

# diagrama de fiabilitat de la probabilitat
rel <- b_rf[, .(n = .N, obs = mean(obs), pred = mean(pred)),
            by = .(bin = cut(pred, seq(0, 1, 0.1), include.lowest = TRUE))]
p19b <- ggplot(rel, aes(pred, obs)) +
  geom_abline(slope = 1, colour = "grey60", linetype = 2) +
  geom_line(colour = "#B2182B") + geom_point(aes(size = n)) +
  scale_size_continuous("nits", range = c(1, 5)) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Probabilitat pronosticada", y = "Frequencia observada",
       title = "Fiabilitat de la probabilitat de Saligarda",
       subtitle = sprintf("AUC = %.3f | Brier = %.3f",
                          suppressMessages(auc(roc(b_rf$obs, b_rf$pred, quiet = TRUE))),
                          mean((b_rf$pred - b_rf$obs)^2))) + tema
ggsave("figures/F20_fiabilitat.png", p19b, width = 5.4, height = 5.6, dpi = 150)

p19c <- ggplot(head(IM, 14), aes(reorder(variable, rel), rel)) +
  geom_col(fill = "#2166AC") + coord_flip() +
  labs(x = NULL, y = "Importancia relativa (%)",
       title = "Que aporta cada predictor",
       subtitle = "Importancia per permutacio, bosc aleatori sobre u_dv") + tema
ggsave("figures/F21_importancia.png", p19c, width = 6.5, height = 4.6, dpi = 150)

fwrite(m_rf, "derived/pronostic_validacio.csv")
fwrite(IM,   "derived/pronostic_importancia.csv")
cat("\n-> figures F19, F20, F21 i derived/pronostic_*.csv\n")
