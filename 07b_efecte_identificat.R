# ==============================================================================
# 07b_efecte_identificat.R -- Separar l'efecte del vent de l'efecte del cel ras
#   Problema: els dies SENSE Saligarda son sobretot dies ennuvolats o de vent
#   sinoptic, que ja de per si tenen matinades mes calides. La comparacio
#   directa confon el vent amb la situacio meteorologica que el genera.
#
#   (d) Comparacio restringida: nomes nits anticiclonqiues i seques; dins
#       d'aquest subconjunt, drenatge fort vs drenatge fluix.
#   (e) Model amb la temperatura simultania a Vic com a control del refredament
#       radiatiu regional: aixi el coeficient del vent mesura el TRANSPORT.
#   (f) Acoblament Garriga-Vic: si la Saligarda es adveccio del fred de la
#       Plana de Vic, les dues estacions han d'anar molt mes lligades.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

g   <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
dia <- fread("derived/saligarda_diari.csv"); dia[, date := as.IDate(date)]
vic <- fread("vic.csv"); vic[, datetime := as.POSIXct(datetime, tz = "UTC")]
vic[, `:=`(date = as.IDate(datetime), h = hour(datetime) + minute(datetime)/60)]

td <- function(T, HR) { a <- 17.625; b <- 243.04
  gm <- log(pmax(HR,1)/100) + a*T/(b+T); b*gm/(a-gm) }

# matinada a la Garriga (03-08 UTC)
gm <- g[h >= 3 & h < 8, .(T_gar = mean(temp_out, na.rm=TRUE),
                          HR_gar = mean(out_hum, na.rm=TRUE),
                          WC_gar = mean(wc, na.rm=TRUE),
                          gap_gar = mean(wc_gap, na.rm=TRUE)), by = date]
gm[, Td_gar := td(T_gar, HR_gar)]
# el mateix a Vic + amplitud termica diaria (indicador de cel ras)
vm <- vic[h >= 3 & h < 8, .(T_vic = mean(T, na.rm=TRUE),
                            HR_vic = mean(HR, na.rm=TRUE)), by = date]
va <- vic[, .(amp_vic = max(T, na.rm=TRUE) - min(T, na.rm=TRUE),
              ppt_vic = sum(PPT, na.rm=TRUE)), by = date]
va[is.infinite(amp_vic), amp_vic := NA]

D <- Reduce(function(a,b) merge(a,b,by="date"), list(dia, gm, vm, va))
D[, `:=`(doy = yday(date), s1 = sin(2*pi*yday(date)/365), c1 = cos(2*pi*yday(date)/365),
         s2 = sin(4*pi*yday(date)/365), c2 = cos(4*pi*yday(date)/365))]
D[, est := factor(est, levels = c("DJF","MAM","JJA","SON"))]
D[, pluja_ahir := shift(pluja)]

cat("=== Comprovacio del confusor: com son els dies sense Saligarda? ===\n")
print(D[, .(dies = .N,
            amp_vic = round(mean(amp_vic, na.rm=TRUE),1),
            bar = round(mean(bar_mitja, na.rm=TRUE),1),
            frac_pluja = round(mean(pluja > 0.2, na.rm=TRUE),3)),
        by = .(saligarda)][order(-saligarda)])
cat("  (amplitud termica alta i pressio alta = cel ras: la Saligarda hi va lligada)\n")

# ============================== (d) dins de nits anticiclonqiues i seques
sub <- D[pluja == 0 & (is.na(pluja_ahir) | pluja_ahir == 0) &
         bar_mitja >= quantile(bar_mitja, 0.5, na.rm=TRUE) &
         amp_vic >= quantile(amp_vic, 0.5, na.rm=TRUE)]
cat(sprintf("\n=== (d) Nits seques, anticiclonqiues i de cel ras: %d dies ===\n", nrow(sub)))
sub[, terc := cut(u_dv, quantile(u_dv, c(0,1/3,2/3,1), na.rm=TRUE),
                  labels = c("drenatge fluix","intermedi","drenatge fort"),
                  include.lowest = TRUE), by = est]
print(sub[!is.na(terc), .(dies = .N,
          u_dv = round(mean(u_dv),1), T = round(mean(T_gar, na.rm=TRUE),2),
          WC = round(mean(WC_gar, na.rm=TRUE),2),
          HR = round(mean(HR_gar, na.rm=TRUE),1), Td = round(mean(Td_gar, na.rm=TRUE),2),
          T_vic = round(mean(T_vic, na.rm=TRUE),2)),
      by = .(est, terc)][order(est, terc)])
cat("\n  contrast fort - fluix dins d'aquestes nits:\n")
for (e in levels(D$est)) {
  a <- sub[est == e & terc == "drenatge fort"]; b <- sub[est == e & terc == "drenatge fluix"]
  if (nrow(a) < 10 || nrow(b) < 10) next
  for (v in c("T_gar","WC_gar","HR_gar","Td_gar")) {
    p <- suppressWarnings(wilcox.test(a[[v]], b[[v]])$p.value)
    cat(sprintf("   %s %-7s: %+6.2f  (p = %s)\n", e, sub("_gar","",v),
                mean(a[[v]], na.rm=TRUE) - mean(b[[v]], na.rm=TRUE), format.pval(p, digits=2)))
  }
}

# ============ (e) model amb la T simultania a Vic: aisla el transport
cat("\n=== (e) Efecte de la intensitat del drenatge amb Vic com a control ===\n")
cat("    T_garriga ~ u_dv + T_vic + amplitud_vic + cicle anual + pressio + any\n")
cat("    coeficient de u_dv = variacio per cada km/h de component vall avall\n")
for (v in c("T_gar","WC_gar","HR_gar","Td_gar","gap_gar")) {
  m <- lm(as.formula(paste(v, "~ u_dv + T_vic + amp_vic + s1 + c1 + s2 + c2 + bar_mitja + any")),
          data = D)
  s <- summary(m)$coefficients["u_dv", ]; ci <- confint(m)["u_dv", ]
  cat(sprintf("   %-7s: %+6.3f per km/h  IC95%% [%+.3f, %+.3f]  p = %s  (R2 = %.2f)\n",
              sub("_gar","",v), s[1], ci[1], ci[2], format.pval(s[4], digits=2),
              summary(m)$r.squared))
}
cat("\n   equivalent per a un episodi tipic d'hivern (u_dv = 14 km/h vs 2 km/h):\n")
for (v in c("T_gar","WC_gar","HR_gar","Td_gar","gap_gar")) {
  m <- lm(as.formula(paste(v, "~ u_dv + T_vic + amp_vic + s1 + c1 + s2 + c2 + bar_mitja + any")),
          data = D[est == "DJF"])
  s <- summary(m)$coefficients["u_dv", 1]
  cat(sprintf("   %-7s: %+6.2f\n", sub("_gar","",v), s * 12))
}

# ============================================ (f) acoblament Garriga - Vic
cat("\n=== (f) Acoblament termic Garriga-Vic a la matinada ===\n")
aco <- D[!is.na(T_gar) & !is.na(T_vic), {
  m <- lm(T_gar ~ T_vic)
  .(dies = .N, r = cor(T_gar, T_vic), pendent = coef(m)[2],
    desnivell = mean(T_gar - T_vic), sd_dif = sd(T_gar - T_vic))
}, by = .(est, saligarda)]
print(aco[order(est, -saligarda)][, lapply(.SD, function(x)
  if (is.numeric(x)) round(x, 3) else x)])
cat("\n  r mes alt i sd de la diferencia mes baixa amb Saligarda = les dues\n")
cat("  estacions respiren la mateixa massa d'aire (adveccio des de la Plana de Vic)\n")

# ==================================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")

p7 <- ggplot(sub[!is.na(terc)], aes(terc, T_gar, fill = terc)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3, linewidth = 0.3) +
  facet_wrap(~est, scales = "free_y", nrow = 1) +
  scale_fill_brewer(NULL, palette = "RdYlBu", direction = -1) +
  labs(x = NULL, y = "Temperatura mitjana 03-08 UTC (ºC)",
       title = "Efecte del drenatge dins de nits comparables",
       subtitle = "Nomes nits seques, anticiclonqiues i de cel ras (amplitud termica alta a Vic)") +
  tema + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave("figures/F7_efecte_dins_nits_clares.png", p7, width = 9, height = 4, dpi = 150)

p8 <- ggplot(D[!is.na(T_vic)], aes(T_vic, T_gar, colour = saligarda)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey70", linetype = 2) +
  geom_point(alpha = 0.18, size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.9) +
  facet_wrap(~est, nrow = 1) +
  scale_colour_manual(NULL, values = c("FALSE" = "#2166AC", "TRUE" = "#B2182B"),
                      labels = c("sense Saligarda", "amb Saligarda")) +
  labs(x = "Temperatura a Vic, 03-08 UTC (ºC)", y = "Temperatura a la Garriga (ºC)",
       title = "Contrast termic entre la Plana de Vic i la Garriga a la matinada",
       subtitle = paste("Amb Saligarda la recta puja: per a la mateixa T a Vic, la Garriga es",
                        "mes calida (+1,4 a +2,2 ºC).\nDiscontinua: identitat.",
                        "L'acoblament (r) millora a MAM, JJA i SON, pero no al DJF.")) +
  tema
ggsave("figures/F8_acoblament_vic.png", p8, width = 10, height = 3.8, dpi = 150)

fwrite(D, "derived/matinades_completes.csv")
cat("\n-> figures/F7_efecte_dins_nits_clares.png, figures/F8_acoblament_vic.png\n")
