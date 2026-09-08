# ==============================================================================
# 01_build_vic.R  --  Reconstrueix el dataset de VIC (XEMA, estacio XO)
#   Corregeix el bug de dataset_maker_vic.R, que escrivia el resultat a
#   "saligarda.csv" i, per tant, va sobreescriure les dades de la Garriga.
#   Sortida: vic.csv  (mai saligarda.csv)
# ==============================================================================
suppressMessages({library(data.table); library(lubridate)})

fitxer_raw <- "Dades_meteorològiques_de_la_XEMA_20251222.csv"
stopifnot(file.exists(fitxer_raw))

raw <- fread(fitxer_raw, colClasses = "character", showProgress = FALSE)
cat("Files llegides:", nrow(raw), "| estacions:", paste(unique(raw$CODI_ESTACIO), collapse=","), "\n")

# --- data/hora: format XEMA "27/10/2018 12:00:00 PM" (hora UTC) -------------
Sys.setlocale("LC_TIME", "C")
raw[, datetime := as.POSIXct(DATA_LECTURA, format = "%d/%m/%Y %I:%M:%S %p", tz = "UTC")]
cat("Dates no parsejades:", sum(is.na(raw$datetime)), "\n")

# --- valor: decimal amb coma -> punt ----------------------------------------
raw[, valor := as.numeric(gsub(",", ".", trimws(VALOR_LECTURA)))]
raw[, codi := as.integer(CODI_VARIABLE)]

# codis presents a XO: 3 HRx | 30 VV10 | 31 DV10 | 32 T | 33 HR | 35 PPT |
#                      36 RS | 40 Tx | 42 Tn | 44 HRn | 50 VVx10 | 51 DVVx10 | 72 PPTx1min
mapa <- c("3"="HRx", "30"="VV10", "31"="DV10", "32"="T", "33"="HR", "34"="P",
          "35"="PPT", "36"="RS", "40"="Tx", "42"="Tn", "44"="HRn",
          "50"="VVx10", "51"="DVVx10", "72"="PPTx1min")

raw <- raw[!is.na(datetime) & !is.na(codi) & CODI_VARIABLE %in% names(mapa)]
raw[, var := mapa[CODI_VARIABLE]]

# marca de validacio: V = validada, T = tecnica/provisional, "" = sense validar
vic <- dcast(raw, datetime ~ var, value.var = "valor", fun.aggregate = mean)
estat <- raw[, .(frac_validat = mean(CODI_ESTAT == "V")), by = datetime]
vic <- merge(vic, estat, by = "datetime", all.x = TRUE)

setorder(vic, datetime)
fwrite(vic, "vic.csv")

cat("\n-> vic.csv escrit:", nrow(vic), "registres\n")
cat("   periode:", format(min(vic$datetime)), "->", format(max(vic$datetime)), "\n")
cat("   columnes:", paste(names(vic), collapse=", "), "\n\n")
cat("--- registres no-NA per variable i any ---\n")
vic[, any := year(datetime)]
print(vic[, lapply(.SD, function(x) sum(!is.na(x))), by = any,
          .SDcols = intersect(c("T","HR","PPT","RS","VV10","DV10","VVx10","DVVx10"), names(vic))])
