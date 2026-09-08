# ==============================================================================
# 00_run_all.R -- Circuit complet de l'estudi de la Saligarda
#
#   Dades d'entrada (no es modifiquen mai):
#     Saligarda/Dades/Dades/<any>/<mes>/*.xls   exports diaris de la Davis
#     Saligarda/Dades/1. El Temps Ajunt LG...   registre diari de l'Ajuntament
#     Dades_meteorologiques_de_la_XEMA_*.csv    estacio XO (Vic), Meteocat
#
#   Sortides: derived/*.csv i figures/*.png
#
#   Nota: 01 i 02 son lents (llegeixen 181 MB de CSV i 4.710 Excel). Un cop
#   generats derived/la_garriga_5min.csv i vic.csv es poden ometre.
#
#   Us:  Rscript 00_run_all.R          (tot)
#        Rscript 00_run_all.R rapid    (salta la reconstruccio de dades)
# ==============================================================================
args <- commandArgs(trailingOnly = TRUE)
rapid <- length(args) > 0 && args[1] == "rapid"

passos <- c(
  "01_build_vic.R",           # XEMA cru        -> vic.csv
  "02_build_garriga.R",       # 4.710 Excel     -> derived/la_garriga_5min.csv
  "03_build_ajuntament.R",    # full Ajuntament -> derived/la_garriga_diari_ajuntament.csv
  "04_qc_temps.R",            # control de la referencia horaria
  "04b_qc_solar.R",           # id., amb geometria solar
  "05_climatologia_vent.R",   # eix, roses, cicle diari, sensacio de fred
  "06_index_saligarda.R",     # index diari i classificacio dels episodis
  "07_efecte_T_HR.R",         # efecte sobre T, HR, punt de rosada i sensacio
  "07b_efecte_identificat.R", # el mateix, controlant el cel ras
  "08_sinoptica.R",           # situacio regional
  "08b_intensitat.R",         # que determina la forca d'un episodi
  "09_tendencies.R",          # tendencies (serie homogenia i serie llarga)
  "09b_homogeneitat.R",       # ruptura del 2011 a la serie llarga
  "11_robustesa.R"            # proves de sensibilitat
)
# 10a/10b (reanalisi NCEP) van a part: la descarrega triga ~1 h i es cacheja
if (rapid) passos <- setdiff(passos, c("01_build_vic.R", "02_build_garriga.R"))

# Els scripts es llegeixen amb source() a l'entorn global i hi defineixen
# variables de nom curt (p, g, D...). Per aixo les del bucle porten punt
# inicial: altrament el "p" del bucle acaba sent un objecte ggplot.
for (.i in seq_along(passos)) {
  .script <- passos[.i]
  .t0 <- Sys.time()
  cat("\n", strrep("=", 78), "\n== ", .script, "\n", strrep("=", 78), "\n", sep = "")
  source(.script, echo = FALSE)
  cat(sprintf("\n[%s: %.1f s]\n", .script,
              as.numeric(difftime(Sys.time(), .t0, units = "secs"))))
}
cat("\nFet. Resultats a derived/ i figures/\n")
