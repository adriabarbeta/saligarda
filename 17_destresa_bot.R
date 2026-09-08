# ==============================================================================
# 17_destresa_bot.R -- Quin encert tindria un bot que ho anuncia cada dia?
#
#   L'encert depen de dues coses que cal fixar:
#     1. QUE es considera Saligarda (el llindar de u_dv)
#     2. A partir de quina probabilitat el bot diu que si
#
#   Compte amb el "% d'encert": amb llindars alts la Saligarda es rara i un bot
#   que digues sempre que NO ja encertaria el 90 %. Per aixo aqui es donen
#   tambe la precisio (dels dies que anuncia, quants passen), la cobertura
#   (dels episodis reals, quants anuncia) i el Peirce, que si descompta l'atzar.
#
#   Validacio deixant un any sencer fora, amb els predictors d'Open-Meteo.
# ==============================================================================
suppressMessages({library(data.table); library(ranger); library(pROC)})
set.seed(23)

M  <- readRDS("derived/model_operatiu.rds")
OM <- readRDS("derived/openmeteo_arxiu.rds")
P  <- M$fes_predictors(OM, clim_rad = M$clim_rad)
D  <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]

d <- merge(D[, .(date, any, est, u_dv, R)], P, by = "date")
d <- d[any %in% 2013:2023]
OMV <- grep("^om_", names(d), value = TRUE)
EST <- c("s1","c1","s2","c2")
d <- d[complete.cases(d[, c("u_dv","R", OMV), with = FALSE])]
cat(sprintf("=== %d nits, %d-%d ===\n\n", nrow(d), min(d$any), max(d$any)))

anys <- sort(unique(d$any))
avalua <- function(llindar) {
  d[, sal := as.integer(u_dv >= llindar & R >= 0.7)]
  d[, sal_f := factor(sal, levels = c(0, 1))]
  f <- as.formula(paste("sal_f ~", paste(c(EST, OMV), collapse = " + ")))
  cv <- rbindlist(lapply(anys, function(a) {
    m <- ranger(f, d[any != a], num.trees = 600, min.node.size = 5, probability = TRUE)
    data.table(obs = d[any == a, sal],
               pred = predict(m, d[any == a])$predictions[, "1"])
  }))
  base <- mean(cv$obs)
  auc  <- as.numeric(suppressMessages(auc(roc(cv$obs, cv$pred, quiet = TRUE))))
  # per a cada tall de decisio, com quedaria el bot
  taula <- rbindlist(lapply(seq(0.20, 0.80, 0.05), function(tall) {
    p <- cv$pred >= tall; o <- cv$obs == 1
    data.table(tall = tall,
               diu_si   = mean(p),
               encert   = mean(p == o),
               precisio = if (sum(p)) sum(p & o) / sum(p) else NA_real_,
               cobertura= sum(p & o) / sum(o),
               peirce   = sum(p & o)/sum(o) - sum(p & !o)/sum(!o))
  }))
  list(llindar = llindar, base = base, auc = auc, taula = taula,
       sempre_no = 1 - base)
}

for (L in c(8, 10, 12, 15)) {
  r <- avalua(L)
  cat(sprintf("### Saligarda = u_dv >= %d km/h  |  %.0f%% dels dies (%.0f dies/any)  |  AUC = %.3f\n",
              L, 100*r$base, 365*r$base, r$auc))
  cat(sprintf("    un bot que digues sempre NO encertaria el %.0f%%\n", 100*r$sempre_no))
  t <- r$taula[tall %in% c(0.30, 0.40, 0.50, 0.60, 0.70)]
  print(t[, .(tall,
              diu_si    = sprintf("%.0f%%", 100*diu_si),
              encert    = sprintf("%.0f%%", 100*encert),
              precisio  = sprintf("%.0f%%", 100*precisio),
              cobertura = sprintf("%.0f%%", 100*cobertura),
              peirce    = round(peirce, 2))])
  millor <- r$taula[which.max(peirce)]
  cat(sprintf("    tall que maximitza el Peirce: %.2f -> encert %.0f%%, precisio %.0f%%, cobertura %.0f%%\n\n",
              millor$tall, 100*millor$encert, 100*millor$precisio, 100*millor$cobertura))
}
cat("NOTA: aquests numeros surten d'alimentar el model amb ERA5, que es una\n")
cat("analisi. En operatiu s'alimenta amb un PRONOSTIC i la destresa baixa.\n")
cat("18_destresa_real.R ho quantifica amb l'arxiu de prediccions passades.\n")
