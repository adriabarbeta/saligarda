# ==============================================================================
# 07_efecte_T_HR.R -- Efecte de la Saligarda sobre T i HR a la Garriga
#   (a) comparacio directa amb els dies sense Saligarda
#   (b) comparacio ajustada: model lineal que controla el cicle anual, la massa
#       d'aire regional (Tmax del dia anterior a Vic), la pressio i l'any
#   (c) contrast amb Vic: la Garriga menys Vic, hora a hora
#   Hipotesi fisica: l'aire que baixa de la Plana de Vic (500 m) fins a la
#   Garriga (250 m) s'escalfa ~2,5 K per compressio adiabatica i, en desfer la
#   inversio local, deixa matinades MES CALIDES i MOLT MES SEQUES.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

g   <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
g[, datetime := as.POSIXct(datetime, tz = "UTC")]
dia <- fread("derived/saligarda_diari.csv"); dia[, date := as.IDate(date)]
vic <- fread("vic.csv"); vic[, datetime := as.POSIXct(datetime, tz = "UTC")]

# punt de rosada (Magnus)
td <- function(T, HR) {
  a <- 17.625; b <- 243.04
  gm <- log(pmax(HR, 1) / 100) + a * T / (b + T)
  b * gm / (a - gm)
}
g[, t_rosada := td(temp_out, out_hum)]
vic[, `:=`(t_rosada = td(T, HR), date = as.IDate(datetime),
           h = hour(datetime) + minute(datetime) / 60)]

g <- merge(g, dia[, .(date, saligarda, tipus, u_dv, R)], by = "date")
g[, grup := fifelse(saligarda, "amb Saligarda", "sense Saligarda")]
g[, est := factor(est, levels = c("DJF", "MAM", "JJA", "SON"))]

# =============================================================== (a) directa
resum <- g[h >= 3 & h < 8, .(T = mean(temp_out, na.rm = TRUE),
                             HR = mean(out_hum, na.rm = TRUE),
                             Td = mean(t_rosada, na.rm = TRUE),
                             WC = mean(wc, na.rm = TRUE),
                             WCmin = suppressWarnings(min(wc, na.rm = TRUE)),
                             gap = mean(wc_gap, na.rm = TRUE)),
           by = .(date, est, grup)]
resum[is.infinite(WCmin), WCmin := NA]
cat("=== (a) Condicions de matinada (03-08 UTC), comparacio directa ===\n")
tab <- resum[, .(dies = .N, T = round(mean(T, na.rm=TRUE),2),
                 HR = round(mean(HR, na.rm=TRUE),1),
                 Td = round(mean(Td, na.rm=TRUE),2),
                 WC = round(mean(WC, na.rm=TRUE),2),
                 gap = round(mean(gap, na.rm=TRUE),2)), by = .(est, grup)]
print(dcast(melt(tab, id.vars = c("est","grup")), est + variable ~ grup))

cat("\n  contrast (Wilcoxon, amb - sense):\n")
for (e in levels(g$est)) for (v in c("T","HR","Td","WC","gap")) {
  a <- resum[est == e & grup == "amb Saligarda"][[v]]
  b <- resum[est == e & grup == "sense Saligarda"][[v]]
  w <- suppressWarnings(wilcox.test(a, b))
  cat(sprintf("   %s %-3s: %+6.2f   (p = %s)\n", e, v,
              mean(a, na.rm=TRUE) - mean(b, na.rm=TRUE), format.pval(w$p.value, digits = 2)))
}

# =============================================================== (b) ajustada
# massa d'aire regional: Tmax del dia anterior a Vic (poc afectada pel drenatge)
vic_d <- vic[, .(vic_tmax = max(T, na.rm = TRUE), vic_tmin = min(T, na.rm = TRUE),
                 vic_hr = mean(HR, na.rm = TRUE)), by = date]
vic_d[is.infinite(vic_tmax), vic_tmax := NA]; vic_d[is.infinite(vic_tmin), vic_tmin := NA]
vic_d[, vic_tmax_prev := shift(vic_tmax)]

D <- merge(resum, dia[, .(date, bar_mitja, pluja, any, mes, u_dv)], by = "date")
D <- merge(D, vic_d[, .(date, vic_tmax_prev, vic_tmin)], by = "date")
D[, `:=`(doy = yday(date), sal = grup == "amb Saligarda")]
D[, `:=`(s1 = sin(2*pi*doy/365), c1 = cos(2*pi*doy/365),
         s2 = sin(4*pi*doy/365), c2 = cos(4*pi*doy/365))]

cat("\n=== (b) Efecte ajustat (model lineal, tot l'any) ===\n")
cat("    controls: cicle anual (2 harmonics), Tmax del dia anterior a Vic,\n")
cat("              pressio mitjana i any\n")
for (v in c("T","HR","Td","WC","gap")) {
  f <- as.formula(paste(v, "~ sal + s1 + c1 + s2 + c2 + vic_tmax_prev + bar_mitja + any"))
  m <- lm(f, data = D)
  s <- summary(m)$coefficients["salTRUE", ]
  ci <- confint(m)["salTRUE", ]
  cat(sprintf("   %-3s: %+6.2f  IC95%% [%+.2f, %+.2f]  p = %s   (n = %d, R2 = %.2f)\n",
              v, s[1], ci[1], ci[2], format.pval(s[4], digits = 2),
              nobs(m), summary(m)$r.squared))
}
cat("\n   per estacio de l'any:\n")
for (e in levels(D$est)) for (v in c("T","HR","Td","WC","gap")) {
  d <- D[est == e]
  m <- lm(as.formula(paste(v, "~ sal + s1 + c1 + vic_tmax_prev + bar_mitja + any")), data = d)
  s <- summary(m)$coefficients["salTRUE", ]; ci <- confint(m)["salTRUE", ]
  cat(sprintf("   %s %-3s: %+6.2f  IC95%% [%+.2f, %+.2f]  p = %s\n",
              e, v, s[1], ci[1], ci[2], format.pval(s[4], digits = 2)))
}

# =============================================================== (c) vs Vic
g30 <- g[, .(tg = mean(temp_out, na.rm = TRUE), hg = mean(out_hum, na.rm = TRUE)),
         by = .(datetime = as.POSIXct(round(as.numeric(datetime)/1800)*1800,
                                      origin = "1970-01-01", tz = "UTC"),
                date, est, grup)]
cmp <- merge(g30, vic[, .(datetime, tv = T, hv = HR)], by = "datetime")
cmp[, `:=`(dT = tg - tv, dHR = hg - hv, hh = hour(datetime) + minute(datetime)/60)]
dif <- cmp[, .(dT = mean(dT, na.rm = TRUE), dHR = mean(dHR, na.rm = TRUE), n = .N),
           by = .(est, grup, hh)]
cat("\n=== (c) la Garriga menys Vic a les 06 UTC ===\n")
print(dcast(dif[hh == 6, .(est, grup, dT = round(dT,2), dHR = round(dHR,1))],
            est ~ grup, value.var = c("dT","dHR")))

# ================================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")
COL <- c("amb Saligarda" = "#B2182B", "sense Saligarda" = "#2166AC")

cic <- g[, .(T = mean(temp_out, na.rm = TRUE), HR = mean(out_hum, na.rm = TRUE),
             Td = mean(t_rosada, na.rm = TRUE), WC = mean(wc, na.rm = TRUE)),
         by = .(est, grup, hh = floor(h))]
cl <- melt(cic, id.vars = c("est","grup","hh"),
           measure.vars = c("T","WC","Td","HR"), variable.name = "var")
cl[, var := factor(var, levels = c("T","WC","Td","HR"),
                   labels = c("Temperatura (ºC)","Sensacio de fred (ºC)",
                              "Punt de rosada (ºC)","Humitat relativa (%)"))]
p5 <- ggplot(cl, aes(hh, value, colour = grup)) +
  geom_line(linewidth = 0.8) +
  facet_grid(var ~ est, scales = "free_y", switch = "y") +
  scale_colour_manual(NULL, values = COL) +
  scale_x_continuous("Hora (UTC)", breaks = seq(0, 24, 6)) +
  labs(y = NULL, title = "Cicle diari mitja a la Garriga segons hi hagi Saligarda o no",
       subtitle = "2012-2024. Saligarda: component vall avall >= 8 km/h entre 00 i 10 UTC") +
  tema + theme(strip.placement = "outside")
ggsave("figures/F5_cicle_T_HR.png", p5, width = 9.5, height = 8, dpi = 150)

dl <- melt(dif, id.vars = c("est","grup","hh"), measure.vars = c("dT","dHR"),
           variable.name = "var")
dl[, var := factor(var, levels = c("dT","dHR"),
                   labels = c("T: Garriga - Vic (ºC)", "HR: Garriga - Vic (%)"))]
p6 <- ggplot(dl, aes(hh, value, colour = grup)) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_line(linewidth = 0.8) +
  facet_grid(var ~ est, scales = "free_y", switch = "y") +
  scale_colour_manual(NULL, values = COL) +
  scale_x_continuous("Hora (UTC)", breaks = seq(0, 24, 6)) +
  labs(y = NULL, title = "Contrast entre la Garriga (250 m) i Vic (500 m)",
       subtitle = "Diferencia hora a hora. La Plana de Vic es la conca que alimenta el drenatge del Congost.") +
  tema + theme(strip.placement = "outside")
ggsave("figures/F6_garriga_vs_vic.png", p6, width = 9.5, height = 5, dpi = 150)

fwrite(D, "derived/matinades.csv")
cat("\n-> figures/F5_cicle_T_HR.png, figures/F6_garriga_vs_vic.png\n")
