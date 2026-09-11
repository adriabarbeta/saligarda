# ==============================================================================
# 15_model_operatiu.R -- Model de pronostic alimentat nomes amb Open-Meteo
#
#   Substitueix els PC de la reanalisi NCEP per predictors fisics directes, amb
#   la NUVOLOSITAT al capdavant, i per gradients de pressio entre quatre punts,
#   que fan la feina de les components principals sense PCA.
#
#   La clau: aquests predictors surten de l'ARXIU ERA5 per entrenar i del
#   PRONOSTIC per operar, tots dos d'Open-Meteo, amb els mateixos noms i unitats.
#   El desajust entre entrenament i operacio queda reduit a l'error del model
#   numeric, que es l'unic inevitable.
#
#   Es comparen dues versions:
#     OP-A  nomes Open-Meteo            -> automatitzable del tot
#     OP-B  Open-Meteo + estacio propia -> millor, pero necessita el vespre real
#
#   Sortida: derived/model_operatiu.rds (models ajustats + funcio de predictors)
# ==============================================================================
suppressMessages({library(data.table); library(ranger); library(pROC); library(ggplot2)})
set.seed(11)

# Mida del bosc. Amb 800 arbres el fitxer del model fa 24 MB i amb 300 en fa 6,
# i la diferencia de destresa es de 0,003 de R2 i 0,002 d'AUC. Com que el model
# ha de viatjar a un repositori i executar-se en un runner, es despleguen 300.
# S'usen els MATEIXOS valors a la validacio i al model final: el que es valida
# ha de ser exactament el que es desplega.
NUM_TREES <- 300L
MIN_NODE  <- 10L

OM <- readRDS("derived/openmeteo_arxiu.rds")
OM[, `:=`(date = as.IDate(datetime), h = hour(datetime))]

# ---------------------------------------------------------------------------
# Construccio dels predictors. Es una funcio perque el script operatiu (16_)
# la reutilitzi EXACTAMENT igual sobre les dades del pronostic.
# ---------------------------------------------------------------------------
#   clim_rad: taula mes -> percentil 95 de la radiacio diaria. En operatiu s'ha
#   de passar la desada de l'entrenament, perque amb tres dies de pronostic un
#   quantil mensual no vol dir res.
fes_predictors <- function(OM, clim_rad = NULL) {
  OM <- copy(OM)
  OM[, `:=`(date = as.IDate(datetime), h = hour(datetime))]
  # components del vent del model al punt de la Garriga
  OM[, `:=`(u10 = -garriga_wind_speed_10m * sin(garriga_wind_direction_10m * pi/180),
            v10 = -garriga_wind_speed_10m * cos(garriga_wind_direction_10m * pi/180))]
  # gradients regionals de pressio (fan de components principals)
  OM[, `:=`(dp_NS = nord_pressure_msl - sud_pressure_msl,
            dp_OE = oest_pressure_msl - garriga_pressure_msl,
            dp_NG = nord_pressure_msl - garriga_pressure_msl)]

  # --- nit objectiu: 00-10 UTC del dia D
  nit <- OM[h < 10, .(
    om_nub      = mean(garriga_cloud_cover, na.rm = TRUE),
    om_nub_baixa= mean(garriga_cloud_cover_low, na.rm = TRUE),
    om_nub_alta = mean(garriga_cloud_cover_high, na.rm = TRUE),
    om_pmsl     = mean(garriga_pressure_msl, na.rm = TRUE),
    om_t2m      = mean(garriga_temperature_2m, na.rm = TRUE),
    om_rh       = mean(garriga_relative_humidity_2m, na.rm = TRUE),
    om_td       = mean(garriga_dew_point_2m, na.rm = TRUE),
    om_u10      = mean(u10, na.rm = TRUE),
    om_v10      = mean(v10, na.rm = TRUE),
    om_v        = mean(garriga_wind_speed_10m, na.rm = TRUE),
    om_dpNS     = mean(dp_NS, na.rm = TRUE),
    om_dpOE     = mean(dp_OE, na.rm = TRUE),
    om_dpNG     = mean(dp_NG, na.rm = TRUE),
    om_t_nord   = mean(nord_temperature_2m, na.rm = TRUE),
    om_t_sud    = mean(sud_temperature_2m, na.rm = TRUE)), by = date]

  # --- finestra del MATI: 6-11 hora local, que es la que viu la gent i la que
  # prediu el bot. S'hi afegeix la radiacio, que es el que de debo apaga el
  # drenatge: el sol escalfa el fons de vall i desfa la inversio.
  OM[, h_loc := as.integer(format(datetime, tz = "Europe/Madrid", format = "%H"))]
  mati <- OM[h_loc >= 6 & h_loc < 11, .(
    om_m_nub   = mean(garriga_cloud_cover, na.rm = TRUE),
    om_m_nubB  = mean(garriga_cloud_cover_low, na.rm = TRUE),
    om_m_rad   = mean(garriga_shortwave_radiation, na.rm = TRUE),
    om_m_t2m   = mean(garriga_temperature_2m, na.rm = TRUE),
    om_m_pmsl  = mean(garriga_pressure_msl, na.rm = TRUE),
    om_m_u10   = mean(u10, na.rm = TRUE),
    om_m_v10   = mean(v10, na.rm = TRUE),
    om_m_dpNS  = mean(dp_NS, na.rm = TRUE)), by = date]

  # --- vespre del dia D-1: 12-17 UTC (i el dia sencer per a la radiacio)
  vespre <- OM[h >= 12 & h < 17, .(
    om_nub_vespre  = mean(garriga_cloud_cover, na.rm = TRUE),
    om_pmsl_vespre = mean(garriga_pressure_msl, na.rm = TRUE),
    om_t2m_vespre  = mean(garriga_temperature_2m, na.rm = TRUE),
    om_rh_vespre   = mean(garriga_relative_humidity_2m, na.rm = TRUE)), by = date]
  rad <- OM[h < 17, .(om_rad = sum(garriga_shortwave_radiation, na.rm = TRUE)), by = date]
  vespre <- merge(vespre, rad, by = "date")
  vespre[, date := date + 1L]                       # es del dia anterior a la nit

  # --- tendencia baromatrica de 24 h, a les 06 UTC
  p06 <- OM[h == 6, .(date, p06 = garriga_pressure_msl)]
  setorder(p06, date)
  p06[, om_dp24 := p06 - shift(p06)]

  d <- Reduce(function(a, b) merge(a, b, by = "date", all.x = TRUE),
              list(nit, mati, vespre, p06[, .(date, om_dp24)]))
  d[, `:=`(doy = yday(date),
           s1 = sin(2*pi*yday(date)/365), c1 = cos(2*pi*yday(date)/365),
           s2 = sin(4*pi*yday(date)/365), c2 = cos(4*pi*yday(date)/365))]
  # radiacio relativa al maxim del mateix mes: indicador de cel ras comparable
  d[, mes_ := month(date)]
  if (is.null(clim_rad)) {
    d[, om_rad_rel := om_rad / quantile(om_rad, 0.95, na.rm = TRUE), by = mes_]
  } else {
    d <- merge(d, clim_rad, by.x = "mes_", by.y = "mes", all.x = TRUE)
    d[, om_rad_rel := om_rad / rad_p95][, rad_p95 := NULL]
  }
  d[, mes_ := NULL]
  setorder(d, date)
  d[]
}

P <- fes_predictors(OM)
cat(sprintf("=== Predictors Open-Meteo: %d dies, %d-%d ===\n", nrow(P),
            year(min(P$date)), year(max(P$date))))

# ------------------------------------------------- objectius i estacio propia
D <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]
g <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
est_vespre <- g[h >= 12 & h < 17, .(
  st_T = mean(temp_out, na.rm=TRUE), st_HR = mean(out_hum, na.rm=TRUE),
  st_P = mean(bar, na.rm=TRUE), st_udv = mean(u_dv, na.rm=TRUE),
  st_vent = mean(wind_mean, na.rm=TRUE)), by = date]
est_vespre[, date := date + 1L]

d <- Reduce(function(a, b) merge(a, b, by = "date"),
            list(D[, .(date, any, mes, est, u_dv, R, saligarda, wc_min)], P, est_vespre))
d[, `:=`(sal = as.integer(saligarda), sal_f = factor(as.integer(saligarda), levels = c(0,1)))]
# ------------------------------------------------- objectiu del bot public
# L'estudi usa la finestra 00-10 UTC, que descriu l'episodi sencer. El bot,
# en canvi, ha de parlar del tram que viu la gent: 6-11 hora local. Provat
# empiricament, entrenar directament sobre el mati prediu el mati millor que
# fer servir el model nocturn (R2 0,518 contra 0,500), encara que el mati
# sigui intrinsecament mes dificil que la nit sencera (0,58).
# El llindar puja de 12 a 14 km/h perque la finestra del mati exclou les
# hores fluixes de la matinada; amb 14 surten uns 48 dies l'any.
LLINDAR_BOT <- 14
gm <- fread("derived/garriga_treball.csv")
gm[, datetime := as.POSIXct(datetime, tz = "UTC")]
gm[, h_loc := as.integer(format(datetime, tz = "Europe/Madrid", format = "%H"))]
# A l'estiu el drenatge es apagat a les 9 i girat a les 10, de manera que la
# mitjana de 6-11 el dilueix (6,2 km/h contra 9,1 del tram 6-9). Canviar la
# finestra costaria destresa (R2 0,533 -> 0,489), aixi que es mante i s'afegeix
# a part la intensitat del tram fort, per poder-ho dir al post.
idx_primera <- gm[h_loc >= 6 & h_loc < 9, .(
  u_primera = mean(u_dv, na.rm = TRUE), n_p = sum(!is.na(u_dv))),
  by = .(date = as.IDate(date))][n_p >= 24][, n_p := NULL]
idx_mati <- gm[h_loc >= 6 & h_loc < 11, .(
  u_mati = mean(u_dv, na.rm = TRUE),
  R_mati = {s <- sin(hi_dir_deg*pi/180); c <- cos(hi_dir_deg*pi/180)
            ok <- !is.na(s) & !is.na(wind_mean) & wind_mean >= 2
            if (sum(ok) < 10) NA_real_ else sqrt(mean(s[ok])^2 + mean(c[ok])^2)},
  n_mati = sum(!is.na(u_dv))), by = .(date = as.IDate(date))][n_mati >= 40]
d <- merge(d, idx_mati, by = "date", all.x = TRUE)
d <- merge(d, idx_primera, by = "date", all.x = TRUE)
d[, sal12_f := factor(as.integer(u_mati >= LLINDAR_BOT & R_mati >= 0.7), levels = c(0,1))]
d <- d[any %in% 2013:2023]
OMV <- grep("^om_", names(d), value = TRUE)
STV <- grep("^st_", names(d), value = TRUE)
EST <- c("s1","c1","s2","c2")
d_tot <- d[complete.cases(d[, c("u_dv", OMV), with = FALSE])]   # model final
d_bot <- d[complete.cases(d[, c("u_mati", "sal12_f", OMV), with = FALSE])]  # bot
d     <- d[complete.cases(d[, c("u_dv", OMV, STV), with = FALSE])]  # comparar A i B
cat(sprintf("    nits per comparar OP-A/OP-B: %d | per al model final: %d | Saligarda: %.1f%%\n",
            nrow(d), nrow(d_tot), 100*mean(d$sal)))

# ================================================= validacio deixant un any fora
anys <- sort(unique(d$any))
prediu <- function(vars, objectiu = "u_dv", tipus = "rf") {
  rbindlist(lapply(anys, function(a) {
    tr <- d[any != a]; te <- d[any == a]
    if (objectiu == "sal") {
      f <- as.formula(paste("sal_f ~", paste(vars, collapse = " + ")))
      p <- if (tipus == "rf")
        predict(ranger(f, tr, num.trees = NUM_TREES, min.node.size = MIN_NODE, probability = TRUE), te)$predictions[, "1"]
      else predict(glm(as.formula(paste("sal ~", paste(vars, collapse = "+"))), tr,
                       family = binomial()), te, type = "response")
      data.table(any = a, obs = te$sal, pred = as.numeric(p), est = te$est)
    } else {
      f <- as.formula(paste(objectiu, "~", paste(vars, collapse = " + ")))
      p <- if (tipus == "rf") predict(ranger(f, tr, num.trees = NUM_TREES, min.node.size = MIN_NODE), te)$predictions
           else predict(lm(f, tr), te)
      data.table(any = a, obs = te[[objectiu]], pred = as.numeric(p), est = te$est)
    }
  }))
}
REF <- NULL
metr <- function(cv, nom, ref = FALSE) {
  mse <- mean((cv$obs - cv$pred)^2); if (ref) REF <<- mse
  cat(sprintf("  %-40s R2 = %6.3f | RMSE = %4.2f | destresa = %+.3f\n", nom,
              1 - sum((cv$obs-cv$pred)^2)/sum((cv$obs-mean(cv$obs))^2),
              sqrt(mse), 1 - mse/REF))
}
BREF <- NULL
metr_bin <- function(cv, nom, ref = FALSE) {
  br <- mean((cv$pred - cv$obs)^2); if (ref) BREF <<- br
  a <- suppressMessages(auc(roc(cv$obs, cv$pred, quiet = TRUE)))
  p <- cv$pred >= 0.5; o <- cv$obs == 1
  cat(sprintf("  %-40s AUC = %.3f | Brier = %.3f (destresa %+.3f) | POD = %.2f | FAR = %.2f\n",
              nom, a, br, 1 - br/BREF, sum(p&o)/sum(o), sum(p&!o)/sum(p)))
}

cat("\n=== A) INTENSITAT ===\n")
metr(prediu(EST, "u_dv", "lm"), "climatologia", TRUE)
metr(prediu(c(EST, OMV), "u_dv", "lm"), "OP-A lineal (nomes Open-Meteo)")
metr(prediu(c(EST, OMV), "u_dv", "rf"), "OP-A bosc (nomes Open-Meteo)")
metr(prediu(c(EST, OMV, STV), "u_dv", "rf"), "OP-B bosc (Open-Meteo + estacio)")

cat("\n=== B) OCURRENCIA ===\n")
metr_bin(prediu(EST, "sal", "lm"), "climatologia", TRUE)
metr_bin(prediu(c(EST, OMV), "sal", "lm"), "OP-A lineal")
b_a <- prediu(c(EST, OMV), "sal", "rf");        metr_bin(b_a, "OP-A bosc")
b_b <- prediu(c(EST, OMV, STV), "sal", "rf");   metr_bin(b_b, "OP-B bosc")

cat("\n=== C) SENSACIO DE FRED MINIMA ===\n")
metr(prediu(EST, "wc_min", "lm"), "climatologia", TRUE)
w_a <- prediu(c(EST, OMV), "wc_min", "rf");     metr(w_a, "OP-A bosc")

a_int <- prediu(c(EST, OMV), "u_dv", "rf")
cat("\n  OP-A per estacio de l'any:\n")
print(a_int[, .(nits = .N, R2 = round(1 - sum((obs-pred)^2)/sum((obs-mean(obs))^2), 3),
                RMSE = round(sqrt(mean((obs-pred)^2)), 2)), by = est][order(est)])

imp <- ranger(as.formula(paste("u_dv ~", paste(c(EST, OMV), collapse = " + "))),
              d, num.trees = 800, importance = "permutation")
IM <- data.table(variable = names(imp$variable.importance),
                 rel = 100 * as.numeric(imp$variable.importance) / max(imp$variable.importance))
setorder(IM, -rel)
cat("\n=== Importancia dels predictors (OP-A) ===\n")
print(head(IM[, .(variable, importancia = round(rel, 1))], 12))

# =============================== validacio del model del BOT (finestra del mati)
cat("
=== D) BOT: tram de 6 a 11 hora local ===
")
anys_b <- sort(unique(d_bot$any))
cv_bot <- rbindlist(lapply(anys_b, function(a) {
  tr <- d_bot[any != a]; te <- d_bot[any == a]
  mi <- ranger(as.formula(paste("u_mati ~", paste(c(EST, OMV), collapse = "+"))),
               tr, num.trees = NUM_TREES, min.node.size = MIN_NODE)
  mo <- ranger(as.formula(paste("sal12_f ~", paste(c(EST, OMV), collapse = "+"))),
               tr, num.trees = NUM_TREES, min.node.size = MIN_NODE, probability = TRUE)
  data.table(o = te$u_mati, p = predict(mi, te)$predictions,
             os = as.integer(as.character(te$sal12_f)),
             ps = predict(mo, te)$predictions[, "1"])
}))
R2_BOT <- 1 - sum((cv_bot$o - cv_bot$p)^2) / sum((cv_bot$o - mean(cv_bot$o))^2)
AUC_BOT <- as.numeric(suppressMessages(auc(roc(cv_bot$os, cv_bot$ps, quiet = TRUE))))
pb <- cv_bot$ps >= 0.5; ob <- cv_bot$os == 1
cat(sprintf("  intensitat  R2 = %.3f | RMSE = %.2f
", R2_BOT,
            sqrt(mean((cv_bot$o - cv_bot$p)^2))))
cat(sprintf("  ocurrencia AUC = %.3f | Brier = %.3f | precisio = %.0f%% | cobertura = %.0f%%
",
            AUC_BOT, mean((cv_bot$ps - cv_bot$os)^2),
            100*sum(pb & ob)/max(1, sum(pb)), 100*sum(pb & ob)/sum(ob)))
cat(sprintf("  episodis (u_mati >= %d km/h): %.0f dies/any
",
            LLINDAR_BOT, 365 * mean(ob)))

# ============================================ models finals per a l'operativa
final <- list(
  intensitat = ranger(as.formula(paste("u_dv ~", paste(c(EST, OMV), collapse = " + "))),
                      d_tot, num.trees = NUM_TREES, min.node.size = MIN_NODE),
  ocurrencia = ranger(as.formula(paste("sal_f ~", paste(c(EST, OMV), collapse = " + "))),
                      d_tot, num.trees = NUM_TREES, min.node.size = MIN_NODE, probability = TRUE),
  # per al bot public: intensitat i probabilitat del tram 6-11 hora local
  intensitat_bot = ranger(as.formula(paste("u_mati ~", paste(c(EST, OMV), collapse = " + "))),
                          d_bot, num.trees = NUM_TREES, min.node.size = MIN_NODE),
  # tram fort del mati (6-9 local), per avisar quan el gruix es a primera hora
  intensitat_primera = ranger(as.formula(paste("u_primera ~", paste(c(EST, OMV), collapse = " + "))),
                              d_bot[!is.na(u_primera)], num.trees = NUM_TREES,
                              min.node.size = MIN_NODE),
  ocurrencia_bot = ranger(as.formula(paste("sal12_f ~", paste(c(EST, OMV), collapse = " + "))),
                          d_bot, num.trees = NUM_TREES, min.node.size = MIN_NODE, probability = TRUE),
  llindar_bot = LLINDAR_BOT, finestra_bot = "6-11 hora local",
  sensacio   = ranger(as.formula(paste("wc_min ~", paste(c(EST, OMV), collapse = " + "))),
                      d_tot[!is.na(wc_min)], num.trees = NUM_TREES, min.node.size = MIN_NODE),
  vars = c(EST, OMV), punts = NULL,
  fes_predictors = fes_predictors,
  clim_rad = d_tot[, .(rad_p95 = quantile(om_rad, 0.95, na.rm = TRUE)), by = .(mes = month(date))],
  entrenat = Sys.Date(), n_nits = nrow(d_tot),
  # cicle horari tipic dels episodis, per redactar el post (hores UTC)
  cicle = fread("derived/saligarda_diari.csv")[saligarda == TRUE,
            .(pic = as.integer(round(median(pic, na.rm = TRUE))),
              final = as.integer(round(median(final, na.rm = TRUE)))), by = est],
  destresa_bot = list(R2 = R2_BOT, AUC = AUC_BOT),
  destresa = list(intensitat_R2 = 1 - sum((a_int$obs-a_int$pred)^2)/sum((a_int$obs-mean(a_int$obs))^2),
                  ocurrencia_AUC = as.numeric(suppressMessages(auc(roc(b_a$obs, b_a$pred, quiet=TRUE)))),
                  sensacio_RMSE = sqrt(mean((w_a$obs-w_a$pred)^2))))
saveRDS(final, "derived/model_operatiu.rds", compress = "xz")
cat("\n-> derived/model_operatiu.rds\n")

if (!dir.exists("figures")) dir.create("figures")
p <- ggplot(head(IM, 14), aes(reorder(variable, rel), rel)) +
  geom_col(fill = "#1B7837") + coord_flip() +
  labs(x = NULL, y = "Importancia relativa (%)",
       title = "Model operatiu: que aporta cada predictor",
       subtitle = "Nomes variables d'Open-Meteo (arxiu ERA5 per entrenar, pronostic per operar)") +
  theme_bw(base_size = 10)
ggsave("figures/F24_importancia_operatiu.png", p, width = 6.8, height = 4.6, dpi = 150)
cat("-> figures/F24_importancia_operatiu.png\n")
