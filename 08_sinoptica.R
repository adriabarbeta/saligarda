# ==============================================================================
# 08_sinoptica.R -- Situacio regional associada a la Saligarda
#   No hi ha reanalisi ni barometre a Vic, de manera que la situacio sinoptica
#   es caracteritza amb variables d'estacio: pressio i tendencia barometrica a
#   la Garriga, pluja, amplitud termica i irradiancia a Vic (2022 enca).
#   Tambe es separa el drenatge pur del vent del nord d'origen sinoptic.
# ==============================================================================
suppressMessages({library(data.table); library(ggplot2)})

D <- fread("derived/matinades_completes.csv"); D[, date := as.IDate(date)]
D[, est := factor(est, levels = c("DJF","MAM","JJA","SON"))]
g <- fread("derived/garriga_treball.csv"); g[, date := as.IDate(date)]
vic <- fread("vic.csv"); vic[, datetime := as.POSIXct(datetime, tz="UTC")]
vic[, date := as.IDate(datetime)]

# pressio diaria i tendencia (variacio respecte del dia anterior)
pd <- g[, .(bar_dia = mean(bar, na.rm=TRUE),
            bar_amp = diff(range(bar, na.rm=TRUE))), by = date]
pd[, bar_tend := bar_dia - shift(bar_dia)]
# irradiancia diaria a Vic (nomes 2022 enca) com a indicador de cel ras
rs <- vic[!is.na(RS), .(rs_dia = sum(RS, na.rm=TRUE)*1800/1e6), by = date]  # MJ/m2
D <- merge(D, pd, by = "date", all.x = TRUE)
D <- merge(D, rs, by = "date", all.x = TRUE)
# fraccio de la irradiancia respecte del maxim del mateix dia de l'any
D[, rs_rel := rs_dia / quantile(rs_dia, 0.95, na.rm=TRUE), by = .(mes)]

cat("=== Situacio regional: dies amb i sense Saligarda ===\n")
tab <- D[, .(dies = .N,
             pressio = round(mean(bar_dia, na.rm=TRUE),1),
             tendencia = round(mean(bar_tend, na.rm=TRUE),2),
             oscil_diaria = round(mean(bar_amp, na.rm=TRUE),2),
             amplitud_T_Vic = round(mean(amp_vic, na.rm=TRUE),1),
             irrad_rel_Vic = round(mean(rs_rel, na.rm=TRUE),2),
             frac_dies_pluja = round(mean(pluja > 0.2, na.rm=TRUE),3),
             HR_min_Vic = round(mean(HR_vic, na.rm=TRUE),1)),
         by = .(est, saligarda)][order(est, -saligarda)]
print(tab)

cat("\n=== Intensitat de la Saligarda segons la pressio (hivern) ===\n")
h <- D[est == "DJF" & !is.na(bar_dia)]
h[, quintil_P := cut(bar_dia, quantile(bar_dia, seq(0,1,.2), na.rm=TRUE),
                     include.lowest = TRUE, labels = paste0("Q", 1:5))]
print(h[, .(dies = .N, pressio = round(mean(bar_dia),1),
            u_dv = round(mean(u_dv, na.rm=TRUE),1),
            frac_saligarda = round(mean(saligarda),2)), by = quintil_P][order(quintil_P)])
cat(sprintf("\n  correlacio pressio - intensitat (DJF): r = %.3f (p = %s)\n",
            cor(h$bar_dia, h$u_dv, use="complete.obs"),
            format.pval(cor.test(h$bar_dia, h$u_dv)$p.value, digits=2)))

cat("\n=== Els dos regims de vent del nord ===\n")
D[, regim := fcase(!saligarda, "sense Saligarda",
                   tipus == "drenatge pur", "drenatge pur",
                   tipus == "nord sinoptic", "nord sinoptic",
                   default = "intermedi")]
print(D[, .(dies = .N,
            u_dv_nit = round(mean(u_dv, na.rm=TRUE),1),
            u_dv_tarda = round(mean(u_dv_tarda, na.rm=TRUE),1),
            ratxa_max = round(mean(ratxa_max, na.rm=TRUE),1),
            pressio = round(mean(bar_dia, na.rm=TRUE),1),
            tendencia = round(mean(bar_tend, na.rm=TRUE),2),
            T_matinada = round(mean(T_gar, na.rm=TRUE),1),
            HR_matinada = round(mean(HR_gar, na.rm=TRUE),1)),
        by = regim][order(-u_dv_nit)])

# ==================================================================== FIGURES
if (!dir.exists("figures")) dir.create("figures")
tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text = element_text(face = "bold"), legend.position = "bottom")

vars <- c(bar_dia = "Pressio mitjana (hPa)", amp_vic = "Amplitud termica a Vic (ºC)",
          rs_rel = "Irradiancia relativa a Vic", bar_tend = "Tendencia barometrica (hPa/dia)")
L <- melt(D[, c("est","saligarda", names(vars)), with = FALSE],
          id.vars = c("est","saligarda"))
L[, variable := factor(variable, levels = names(vars), labels = vars)]
L[, grup := fifelse(saligarda, "amb Saligarda", "sense Saligarda")]
p9 <- ggplot(L[!is.na(value)], aes(est, value, fill = grup)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.25, linewidth = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(NULL, values = c("amb Saligarda" = "#B2182B",
                                     "sense Saligarda" = "#2166AC")) +
  labs(x = NULL, y = NULL, title = "Situacio regional associada a la Saligarda",
       subtitle = paste0("Pressio i amplitud termica a Vic mes altes amb Saligarda a les ",
                         "quatre estacions;\nel senyal d'irradiancia nomes es net a ",
                         "l'hivern i la tardor")) +
  tema
ggsave("figures/F9_sinoptica.png", p9, width = 9, height = 6, dpi = 150)

# cicle de vida mitja dels dos regims
g2 <- merge(g, D[, .(date, regim)], by = "date")
g2[, hh := floor(h)]
g2[, nit_date := date + fifelse(hh >= 14, 1L, 0L)]
g2[, h_nit := fifelse(hh >= 14, hh - 24, hh)]
cv <- g2[!is.na(u_dv) & regim != "intermedi",
         .(u = mean(u_dv, na.rm=TRUE)), by = .(regim, h_nit)]
p10 <- ggplot(cv, aes(h_nit, u, colour = regim)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 0, colour = "grey85", linetype = 3) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(NULL, values = c("drenatge pur" = "#B2182B",
                                       "nord sinoptic" = "#E08214",
                                       "sense Saligarda" = "#2166AC")) +
  scale_x_continuous("Hora (UTC; negatiu = vespre del dia anterior)",
                     breaks = seq(-10, 12, 2)) +
  labs(y = "Component vall avall (km/h)",
       title = "Cicle de vida: drenatge pur contra vent del nord sinoptic",
       subtitle = "El drenatge s'apaga a mig mati; el vent sinoptic aguanta tot el dia") +
  tema
ggsave("figures/F10_cicle_vida.png", p10, width = 8, height = 4.5, dpi = 150)

fwrite(D, "derived/dies_sinoptica.csv")
cat("\n-> figures/F9_sinoptica.png, figures/F10_cicle_vida.png\n")
