library(tidyverse)
library(lubridate)

# --- 1. CONFIGURACIÓ ---
Sys.setlocale("LC_TIME", "C") # Important per les dates AM/PM
fitxer_entrada <- "Dades_meteorològiques_de_la_XEMA_20251222.csv"

# --- 2. LECTURA RAW ---
dades_raw <- read.csv(fitxer_entrada, stringsAsFactors = FALSE, colClasses = "character")

cat("Files totals:", nrow(dades_raw), "\n")

# --- 3. TRANSFORMACIÓ MANUAL (LA SOLUCIÓ) ---
dades_clean <- dades_raw %>%
  mutate(
    # 1. DATA: Reconeixement flexible (AM/PM i 24h)
    datetime = parse_date_time(DATA_LECTURA, orders = c("d/m/Y I:M:S p", "dmy HMS", "ymd HMS")),
    date = as.Date(datetime),
    time = format(datetime, "%H:%M:%S"),
    
    # 2. CODI: Convertim a enter
    codi_num = suppressWarnings(as.integer(CODI_VARIABLE)),
    
    # 3. VALOR: EL TRUC DEFINITIU
    # Pas 1: Traiem espais en blanc
    val_txt = trimws(VALOR_LECTURA),
    # Pas 2: Substituïm QUALSEVOL coma per un punt (sigui del 2011 o 2025)
    val_txt = gsub(",", ".", val_txt),
    # Pas 3: Convertim a numèric (R sempre vol punts)
    val_num = suppressWarnings(as.numeric(val_txt))
  ) %>%
  # Filtrem dades vàlides
  filter(!is.na(datetime) & !is.na(codi_num)) %>%
  filter(codi_num %in% c(30, 31, 32, 33, 34, 35)) %>%
  mutate(nom_var = paste0("VAR_", codi_num))

# --- 4. DIAGNÒSTIC RÀPID ---
# Això ens dirà si estem recuperant dades antigues
cat("\n--- RECOMPTE DE DADES PER ANY (EXEMPLE) ---\n")
dades_clean %>%
  mutate(any = year(datetime)) %>%
  group_by(any, nom_var) %>%
  summarise(n = n(), .groups="drop") %>%
  pivot_wider(names_from = nom_var, values_from = n) %>%
  print()

# --- 5. PIVOTATGE ---
dades_pivot <- dades_clean %>%
  pivot_wider(
    id_cols = c(datetime, date, time),
    names_from = nom_var,
    values_from = val_num,
    values_fn = mean
  )

# --- 6. MAPATGE FINAL ---
dades_finals <- dades_pivot %>%
  rename(any_of(c(
    "hi_temp"  = "VAR_32",
    "out_hum"  = "VAR_33",
    "hi_speed" = "VAR_30",
    "dir"      = "VAR_31",
    "rain"     = "VAR_35",
    "bar"      = "VAR_34"
  )))

# Omplir columnes faltants
for(v in c("hi_temp", "out_hum", "hi_speed", "dir", "rain", "bar")) {
  if(!(v %in% names(dades_finals))) dades_finals[[v]] <- NA_real_
}

# --- 7. CÀLCULS I GUARDAR ---
vic_dataset <- dades_finals %>%
  mutate(
    low_temp = hi_temp,
    dew_point = if_else(!is.na(hi_temp) & !is.na(out_hum), 
                        hi_temp - ((100 - out_hum)/5), 
                        NA_real_)
  ) %>%
  arrange(datetime) %>%
  select(datetime, date, time, hi_temp, low_temp, out_hum, dew_point, rain, hi_speed, dir, bar)

write.csv(vic_dataset, "saligarda.csv", row.names = FALSE)

cat("\n=== ARA MIRA EL HEAD I EL TAIL ===\n")
print(head(saligarda_dataset))
print(tail(saligarda_dataset))

# 2. TAULA DE DIAGNÒSTIC PER ANYS
# Això et dirà exactament on tens dades i on no
cat("\n--- DISPONIBILITAT DE DADES PER ANY ---\n")
disponibilitat <- vic_dataset %>%
  mutate(any = year(datetime)) %>%
  group_by(any) %>%
  summarise(
    total_registres = n(),
    dies_amb_temp = n_distinct(date[!is.na(hi_temp)]),
    registres_temp_ok = sum(!is.na(hi_temp)),
    percentatge_valid = round(registres_temp_ok / n() * 100, 1)
  )

print(disponibilitat)

# 3. GRÀFIC DE L'EVOLUCIÓ TEMPORAL
# Si surt una línia plana o buida al principi, és que l'estació no tenia sensor llavors
ggplot(vic_dataset, aes(x = datetime, y = hi_temp)) +
  geom_line(color = "#E67E22", alpha = 0.6, size = 0.3) +
  theme_minimal() +
  labs(
    title = "Històric de Temperatura",
    subtitle = "Si veus espais en blanc, són períodes sense dades al fitxer original",
    y = "Temperatura (ºC)",
    x = "Any"
  ) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
