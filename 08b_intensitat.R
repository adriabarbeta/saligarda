# ==============================================================================
# 08b_intensitat.R -- Que determina la forca d'una Saligarda?
#   Un flux de drenatge s'accelera per la flotabilitat negativa de l'aire fred
#   acumulat a la conca. Aqui es quantifica quina part de la variabilitat de la
#   intensitat nocturna expliquen: (a) el refredament radiatiu (cel ras),
#   (b) l'acumulacio de fred a la Plana de Vic, (c) la pressio i (d) l'estacio.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

D <- fread("derived/dies_sinoptica.csv"); D[, date := as.IDate(date)]
D[, est := factor(est, levels = c("DJF","MAM","JJA","SON"))]
vic <- fread("vic.csv"); vic[, datetime := as.POSIXct(datetime, tz="UTC")]
vic[, `:=`(date = as.IDate(datetime), h = hour(datetime) + minute(datetime)/60)]
g <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]

# estat del vespre anterior (16-19 UTC): condicions inicials de la nit
vesp_v <- vic[h >= 16 & h < 19, .(T_vic_vesp = mean(T, na.rm=TRUE)), by = date]
vesp_g <- g[h >= 16 & h < 19, .(T_gar_vesp = mean(temp_out, na.rm=TRUE)), by = date]
vesp <- merge(vesp_v, vesp_g, by = "date")
vesp[, dT_vesp := T_vic_vesp - T_gar_vesp]     # negatiu: Vic ja mes freda
vesp[, date := date + 1L]                      # s'assigna a la nit seguent

X <- merge(D, vesp, by = "date")
X[, `:=`(s1 = sin(2*pi*yday(date)/365), c1 = cos(2*pi*yday(date)/365),
         s2 = sin(4*pi*yday(date)/365), c2 = cos(4*pi*yday(date)/365))]
X <- X[!is.na(u_dv) & !is.na(amp_vic) & !is.na(bar_mitja) & !is.na(dT_vesp)]

cat("=== Que explica la intensitat nocturna del drenatge? ===\n")
cat("   variable resposta: u_dv (component mitjana vall avall, 00-10 UTC)\n")
cat(sprintf("   n = %d nits\n\n", nrow(X)))

m0 <- lm(u_dv ~ s1 + c1 + s2 + c2, X)
m1 <- update(m0, . ~ . + amp_vic)
m2 <- update(m1, . ~ . + bar_mitja)
m3 <- update(m2, . ~ . + dT_vesp)
for (nm in c("cicle anual", "+ amplitud termica a Vic (cel ras)",
             "+ pressio", "+ contrast Vic-Garriga al vespre")) {
  m <- list(m0, m1, m2, m3)[[match(nm, c("cicle anual", "+ amplitud termica a Vic (cel ras)",
                                         "+ pressio", "+ contrast Vic-Garriga al vespre"))]]
  cat(sprintf("  %-38s R2 = %.3f\n", nm, summary(m)$r.squared))
}
cat("\n  coeficients del model complet:\n")
co <- summary(m3)$coefficients
for (v in c("amp_vic", "bar_mitja", "dT_vesp")) {
  cat(sprintf("   %-12s %+7.4f km/h per unitat  (p = %s)\n", v, co[v,1],
              format.pval(co[v,4], digits = 2)))
}

cat("\n=== Per estacio de l'any ===\n")
for (e in levels(X$est)) {
  d <- X[est == e]
  m <- lm(u_dv ~ amp_vic + bar_mitja + dT_vesp + s1 + c1, d)
  co <- summary(m)$coefficients
  cat(sprintf("  %s (n = %4d, R2 = %.2f): amplitud %+.3f | pressio %+.3f | contrast %+.3f\n",
              e, nrow(d), summary(m)$r.squared,
              co["amp_vic",1], co["bar_mitja",1], co["dT_vesp",1]))
}

cat("\n=== Intensitat segons el refredament radiatiu (amplitud termica a Vic) ===\n")
X[, q_amp := cut(amp_vic, quantile(amp_vic, seq(0,1,.2), na.rm=TRUE),
                 include.lowest = TRUE, labels = paste0("Q", 1:5))]
print(X[, .(nits = .N, amplitud = round(mean(amp_vic),1),
            u_dv = round(mean(u_dv),1),
            frac_saligarda = round(mean(saligarda),2)), by = q_amp][order(q_amp)])

if (!dir.exists("figures")) dir.create("figures")
p <- ggplot(X, aes(amp_vic, u_dv)) +
  geom_point(alpha = 0.12, size = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), colour = "#B2182B",
              fill = "#B2182B", alpha = 0.2) +
  facet_wrap(~est, nrow = 1) +
  labs(x = "Amplitud termica diaria a Vic (ºC)  -  indicador de cel ras",
       y = "Component mitjana vall avall 00-10 UTC (km/h)",
       title = "La Saligarda la mana el cel ras",
       subtitle = "Com mes refredament radiatiu a la Plana de Vic, mes fort baixa el drenatge pel Congost") +
  theme_bw(base_size = 10) +
  theme(strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())
ggsave("figures/F18_intensitat.png", p, width = 9.5, height = 3.8, dpi = 150)
cat("\n-> figures/F18_intensitat.png\n")
