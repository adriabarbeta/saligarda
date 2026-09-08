# ==============================================================================
# 18b_destresa_real.R -- Destresa AMB PRONOSTIC de debo, no amb reanalisi
#   Els numeros de 17_ surten d'alimentar el model amb ERA5, que es una analisi
#   feta a posteriori. En operatiu s'alimenta amb el que el model numeric va
#   predir. Aqui s'entrena amb ERA5 (com el model real) i es prediu amb l'arxiu
#   de prediccions passades: la diferencia es la degradacio que cal esperar.
# ==============================================================================
suppressMessages({library(data.table); library(ranger); library(pROC)})
set.seed(29)

M   <- readRDS("derived/model_operatiu.rds")
ERA <- M$fes_predictors(readRDS("derived/openmeteo_arxiu.rds"), clim_rad = M$clim_rad)
PRO <- M$fes_predictors(readRDS("derived/openmeteo_pronostics_passats.rds"), clim_rad = M$clim_rad)
D   <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]

OMV <- grep("^om_", names(ERA), value = TRUE)
EST <- c("s1","c1","s2","c2")
obj <- D[, .(date, any, u_dv, R)]

era <- merge(obj, ERA, by = "date")[complete.cases(merge(obj, ERA, by="date")[, c("u_dv", OMV), with=FALSE])]
pro <- merge(obj, PRO, by = "date")[complete.cases(merge(obj, PRO, by="date")[, c("u_dv", OMV), with=FALSE])]
anys_test <- intersect(unique(pro$any), 2022:2024)
cat(sprintf("=== Entrenament amb ERA5 (%d nits) | prova amb PRONOSTIC (%d nits, anys %s) ===\n\n",
            nrow(era), nrow(pro[any %in% anys_test]), paste(anys_test, collapse=", ")))

compara <- function(llindar) {
  era[, sal_f := factor(as.integer(u_dv >= llindar & R >= 0.7), levels = c(0,1))]
  pro[, sal   := as.integer(u_dv >= llindar & R >= 0.7)]
  f_i <- as.formula(paste("u_dv ~", paste(c(EST, OMV), collapse = " + ")))
  f_o <- as.formula(paste("sal_f ~", paste(c(EST, OMV), collapse = " + ")))
  out <- rbindlist(lapply(anys_test, function(a) {
    tr <- era[any != a]
    te_p <- pro[any == a]; te_e <- era[any == a]
    if (!nrow(te_p) || !nrow(te_e)) return(NULL)
    mi <- ranger(f_i, tr, num.trees = 600, min.node.size = 5)
    mo <- ranger(f_o, tr, num.trees = 600, min.node.size = 5, probability = TRUE)
    comu <- intersect(te_p$date, te_e$date)
    tp <- te_p[date %in% comu][order(date)]; tt <- te_e[date %in% comu][order(date)]
    data.table(any = a, obs_u = tp$u_dv, obs_s = tp$sal,
               u_pro = predict(mi, tp)$predictions, u_era = predict(mi, tt)$predictions,
               p_pro = predict(mo, tp)$predictions[,"1"], p_era = predict(mo, tt)$predictions[,"1"])
  }))
  r2 <- function(o,p) 1 - sum((o-p)^2)/sum((o-mean(o))^2)
  cat(sprintf("### Llindar %d km/h  (%.0f%% dels dies a la mostra de prova)\n",
              llindar, 100*mean(out$obs_s)))
  cat(sprintf("  intensitat  R2 : amb ERA5 %.3f | amb PRONOSTIC %.3f  (%+.0f%%)\n",
              r2(out$obs_u, out$u_era), r2(out$obs_u, out$u_pro),
              100*(r2(out$obs_u,out$u_pro)/r2(out$obs_u,out$u_era) - 1)))
  ae <- as.numeric(suppressMessages(auc(roc(out$obs_s, out$p_era, quiet=TRUE))))
  ap <- as.numeric(suppressMessages(auc(roc(out$obs_s, out$p_pro, quiet=TRUE))))
  cat(sprintf("  ocurrencia AUC : amb ERA5 %.3f | amb PRONOSTIC %.3f  (%+.3f)\n", ae, ap, ap-ae))
  for (tall in c(0.4, 0.5)) {
    p <- out$p_pro >= tall; o <- out$obs_s == 1
    cat(sprintf("   tall %.1f -> diu si %2.0f%% dels dies | encert %2.0f%% | precisio %2.0f%% | cobertura %2.0f%%\n",
                tall, 100*mean(p), 100*mean(p==o),
                100*sum(p&o)/max(1,sum(p)), 100*sum(p&o)/sum(o)))
  }
  cat("\n")
}
for (L in c(8, 12, 15)) compara(L)
