library(readxl)
library(stringr)
library(purrr)
library(lubridate)
library(ggthemes)
library(forcats)
library(tidyverse)

# --- 1. Load Data ---
# Reading the pre-processed CSV file
# Adjust the path if necessary
csv_path <- "C:/Users/ABARBETA/Documents/Saligarda/saligarda.csv" 
if(!file.exists(csv_path)) {
  # Fallback to relative path if absolute doesn't work for some reason
  csv_path <- "saligarda.csv"
}
meteo_dataset <- read.csv(csv_path, stringsAsFactors = FALSE)

# --- 2. Preprocessing ---

# Convert date/time
if("datetime" %in% names(meteo_dataset)) {
  meteo_dataset <- meteo_dataset %>%
    mutate(datetime = ymd_hms(datetime))
} else if(all(c("date", "time") %in% names(meteo_dataset))) {
  meteo_dataset <- meteo_dataset %>%
    mutate(datetime = ymd_hms(paste(date, time)))
} else {
  stop("No 'datetime' or 'date'+'time' columns found in CSV.")
}

# Extract temporal components
meteo_dataset <- meteo_dataset %>%
  mutate(
    date = as.Date(datetime),
    time_hours = hour(datetime) + minute(datetime)/60,
    time_halfhour = floor(time_hours * 2) / 2
  )

# Season function
get_season <- function(dates) {
  m <- month(dates)
  case_when(
    m %in% c(12,1,2) ~ "DJF",
    m %in% c(3,4,5)  ~ "MAM",
    m %in% c(6,7,8)  ~ "JJA",
    m %in% c(9,10,11) ~ "SON",
    TRUE ~ NA_character_
  )
}

meteo_dataset <- meteo_dataset %>%
  mutate(season = get_season(date))

# Wind direction angles
dir_angles <- c(
  N = 0, NNE = 22.5, NE = 45, ENE = 67.5,
  E = 90, ESE = 112.5, SE = 135, SSE = 157.5,
  S = 180, SSW = 202.5, SW = 225, WSW = 247.5,
  W = 270, WNW = 292.5, NW = 315, NNW = 337.5
)

# --- 3. Saligarda Classification ---

# Step 1: Filter for 0-10h and N/NE/NNE winds > 15km/h
meteo_matinada <- meteo_dataset %>%
  filter(time_hours >= 0 & time_hours <= 10) %>%
  mutate(nord_flag = hi_dir %in% c("N", "NE", "NNE") & hi_speed > 15)

# Step 2: Count hours with condition for each day
saligarda_flag <- meteo_matinada %>%
  group_by(date) %>%
  summarise(
    hours_nord_vent = sum(nord_flag, na.rm = TRUE) * 0.5  # assuming each row is 30 mins
  ) %>%
  mutate(
    saligarda = ifelse(hours_nord_vent >= 5, "si", "no")
  )

# Step 3: Join back to original dataset
meteo_dataset <- meteo_dataset %>%
  left_join(saligarda_flag %>% dplyr::select(date, saligarda), by = "date")

# --- 4. Analysis & Plots ---

# Profile by season, half-hour, and saligarda status
profile_all <- meteo_dataset %>%
  group_by(season, time_halfhour, saligarda) %>%
  summarise(
    hi_speed_mean = mean(hi_speed, na.rm = TRUE),
    hi_speed_sd   = sd(hi_speed, na.rm = TRUE),
    n_obs        = sum(!is.na(hi_speed)),
    hi_dir_mode  = names(sort(table(hi_dir), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  mutate(
    hi_speed_sem = hi_speed_sd / sqrt(n_obs),
    angle_deg = dir_angles[hi_dir_mode],
    angle_rad = pi/180 * angle_deg,
    arrow_length = 1.5,
    xend = time_halfhour - arrow_length * sin(angle_rad),
    yend = hi_speed_mean - arrow_length * cos(angle_rad)
  )

# Plot: Wind Speed Profile
p_wind <- ggplot(profile_all %>% filter(!season == "NA"), aes(x = time_halfhour, y = hi_speed_mean)) +
  geom_ribbon(aes(ymin = hi_speed_mean - hi_speed_sem,
                  ymax = hi_speed_mean + hi_speed_sem),
              fill = "grey70", alpha = 0.3) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_segment(aes(xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red") +
  facet_wrap(~season+saligarda, nrow = 4) +
  scale_x_continuous("Hora del dia", breaks = seq(0,24,2)) +
  scale_y_continuous("Mitjana velocitat vent (km/h)") +
  theme_few() +
  theme(legend.position = "bottom") +
  geom_hline(aes(yintercept=15))

print(p_wind)

# --- 5. Daily Summary Analysis ---

daily_summary <- meteo_dataset %>%
  group_by(date, saligarda, season) %>%
  summarise(
    hi_temp = mean(hi_temp, na.rm = TRUE),
    low_temp = mean(low_temp, na.rm = TRUE),
    out_hum = mean(out_hum, na.rm = TRUE),
    .groups = "drop"
  )

# T-tests
compare_vars <- c("hi_temp", "low_temp", "out_hum")
t_results <- lapply(compare_vars, function(var) {
  # Check if we have enough data for both groups
  if(sum(daily_summary$saligarda == "si", na.rm=TRUE) > 1 && 
     sum(daily_summary$saligarda == "no", na.rm=TRUE) > 1) {
    ttest <- t.test(daily_summary[[var]] ~ daily_summary$saligarda)
    data.frame(
      variable = var,
      mean_saligarda = mean(daily_summary[[var]][daily_summary$saligarda == "si"], na.rm = TRUE),
      mean_no_saligarda = mean(daily_summary[[var]][daily_summary$saligarda == "no"], na.rm = TRUE),
      p_value = ttest$p.value
    )
  } else {
    NULL
  }
}) %>% bind_rows()

print(t_results)

# Boxplots
daily_summary_long <- daily_summary %>% 
  tidyr::pivot_longer(cols = c(hi_temp, low_temp, out_hum),
                      names_to = "variable", values_to = "valor")

p_box <- ggplot(daily_summary_long %>% filter(!saligarda == "NA"), 
       aes(x = saligarda, y = valor, fill = saligarda)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~variable+season, scales = "free_y") +
  theme_few() +
  theme(legend.position = "none") +
  labs(
    title = "Comparació de variables entre dies amb i sense efecte Saligarda",
    x = "Dia amb Saligarda",
    y = "Valor mitjà"
  )

print(p_box)

# --- 9. Comparació de la Pressió Atmosfèrica ---

# 1. Preparem les dades diàries de pressió
# Nota: Si al teu CSV la columna té un altre nom (ex: 'pressure'), canvia 'bar' per aquell nom.
daily_pressure <- meteo_dataset %>%
  group_by(date, season, saligarda) %>%
  summarise(
    pressio_mitjana = mean(bar, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(saligarda) & !is.na(pressio_mitjana))

# 2. T-test global per veure si la diferència és significativa
t_pressio <- t.test(pressio_mitjana ~ saligarda, data = daily_pressure)
print("Resultat del T-test per a la Pressió Atmosfèrica:")
print(t_pressio)

# 3. Visualització de la pressió per estació
p_pressio_comparativa <- ggplot(daily_pressure, aes(x = saligarda, y = pressio_mitjana, fill = saligarda)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
  facet_wrap(~season) +
  theme_few() +
  scale_fill_manual(values = c("no" = "#AED6F1", "si" = "#E67E22")) +
  labs(
    title = "Pressió Atmosfèrica: Dies de Saligarda vs Dies sense",
    subtitle = paste("p-value global:", format.pval(t_pressio$p.value, digits = 3)),
    x = "Hi ha Saligarda?",
    y = "Pressió mitjana (hPa / mm Hg)",
    fill = "Saligarda"
  )

print(p_pressio_comparativa)

# 4. Resum numèric per estació
resum_pressio <- daily_pressure %>%
  group_by(season, saligarda) %>%
  summarise(
    mitjana = mean(pressio_mitjana),
    sd = sd(pressio_mitjana),
    n = n(),
    .groups = "drop"
  )

print(resum_pressio)


# VIC data ----------------------------------------------------------------

# ==============================================================================
# 10. COMPARACIÓ AMB LES DADES DE VIC (Regeneració i Anàlisi)
# ==============================================================================

library(lubridate)
library(tidyverse)
library(readr)
library(ggthemes)

# --- PAS 1: REGENERAR DADES DE VIC DES DE L'ORIGINAL ---
# Utilitzem el fitxer RAW original per assegurar que tenim dades bones
fitxer_vic_raw <- "Dades_meteorològiques_de_la_XEMA_20251222.csv"

if (file.exists(fitxer_vic_raw)) {
  
  cat("Processant fitxer original de Vic: ", fitxer_vic_raw, "...\n")
  
  # Llegim el fitxer cru
  vic_raw <- read.csv(fitxer_vic_raw, stringsAsFactors = FALSE, colClasses = "character")
  
  # Netegem i convertim (Codi Robust validat)
  vic_clean <- vic_raw %>%
    mutate(
      # 1. DATA: Format flexible (detecta AM/PM i 24h)
      datetime = parse_date_time(DATA_LECTURA, orders = c("d/m/Y I:M:S p", "dmy HMS", "ymd HMS", "dmy HM")),
      
      # 2. VALOR: Forcem el decimal a punt '.'
      # parse_number és l'eina més segura: ignora lletres i busca el número
      val_num = parse_number(VALOR_LECTURA, locale = locale(decimal_mark = ",")),
      
      # 3. CODI: Convertim a enter
      codi_num = parse_number(CODI_VARIABLE)
    ) %>%
    filter(!is.na(datetime) & !is.na(codi_num)) %>%
    filter(codi_num %in% c(30, 31, 32, 33, 34, 35)) %>%
    mutate(nom_var = paste0("VAR_", codi_num)) %>%
    # Pivotem a format ample
    pivot_wider(
      id_cols = datetime,
      names_from = nom_var,
      values_from = val_num,
      values_fn = mean
    ) %>%
    # Rebategem columnes
    rename(any_of(c(
      "vic_temp" = "VAR_32",
      "vic_hum"  = "VAR_33",
      "vic_wind" = "VAR_30"
    )))
  
  cat("  -> Dades de Vic recuperades:", nrow(vic_clean), "registres.\n")
  
  # --- PAS 2: PREPARAR UNIÓ AMB LA GARRIGA ---
  
  # Arrodonim les hores a 30 minuts per sincronitzar les dues estacions
  # Això permet creuar una dada de les 10:02 amb una de les 10:00
  vic_ready <- vic_clean %>%
    mutate(datetime_round = round_date(datetime, unit = "30 minutes")) %>%
    select(datetime_round, vic_temp, vic_hum, vic_wind)
  
  garriga_ready <- meteo_dataset %>%
    mutate(
      datetime_round = round_date(datetime, unit = "30 minutes"),
      hora = hour(datetime)
    ) %>%
    # Filtrem pel matí (moment típic de la Saligarda)
    filter(hora < 11) %>%
    select(datetime_round, season, saligarda, gar_temp = hi_temp, gar_hum = out_hum, gar_wind = hi_speed)
  
  # --- PAS 3: UNIÓ I ANÀLISI ---
  
  comparativa <- inner_join(garriga_ready, vic_ready, by = "datetime_round") %>%
    filter(!is.na(saligarda) & !is.na(season))
  
  cat("  -> Dies coincidents per comparar:", nrow(comparativa), "\n")
  
  if (nrow(comparativa) > 0) {
    
    # A) GRÀFIC: Com reacciona Vic quan hi ha Saligarda a La Garriga?
    vic_long <- comparativa %>%
      select(season, saligarda, vic_temp, vic_hum, vic_wind) %>%
      pivot_longer(cols = starts_with("vic_"), names_to = "variable", values_to = "valor") %>%
      mutate(variable = case_when(
        variable == "vic_temp" ~ "Temperatura (ºC)",
        variable == "vic_hum"  ~ "Humitat (%)",
        variable == "vic_wind" ~ "Vent (km/h)",
        TRUE ~ variable
      )) %>%
      filter(!is.na(valor))
    
    p_comp <- ggplot(vic_long, aes(x = saligarda, y = valor, fill = saligarda)) +
      geom_boxplot(outlier.shape = 1, alpha = 0.6) +
      facet_grid(variable ~ season, scales = "free_y") +
      scale_fill_manual(values = c("no" = "#BDC3C7", "si" = "#E74C3C")) +
      theme_few() +
      labs(
        title = "Condicions a Vic quan hi ha Saligarda a La Garriga",
        subtitle = "Comparativa simultània (Matins < 11h)",
        x = "Hi ha Saligarda a La Garriga?", 
        y = "Valors registrats a Vic",
        fill = "Saligarda"
      )
    
    print(p_comp)
    
    # B) TAULA DE DIFERÈNCIES
    cat("\n--- Diferència Mitjana (Vic - La Garriga) en dies de Saligarda ---\n")
    cat("Nota: Valors negatius indiquen que Vic té menys T/HR/Vent que La Garriga.\n")
    
    stats_dif <- comparativa %>%
      filter(saligarda == "si") %>%
      summarise(
        Dif_Temp = mean(vic_temp - gar_temp, na.rm=TRUE),
        Dif_Hum  = mean(vic_hum - gar_hum, na.rm=TRUE),
        Dif_Vent = mean(vic_wind - gar_wind, na.rm=TRUE),
        Dies_Analitzats = n()
      )
    print(stats_dif)
    
  } else {
    cat("\nAVÍS: No s'han trobat dates coincidents.\n")
    cat("Revisa els anys: Vic té dades de:", year(min(vic_clean$datetime)), "a", year(max(vic_clean$datetime)), "\n")
    cat("La Garriga té dades de:", year(min(meteo_dataset$datetime)), "a", year(max(meteo_dataset$datetime)), "\n")
  }
  
} else {
  cat("ERROR: No es troba el fitxer original:", fitxer_vic_raw)
}

# ==============================================================================
# 13. COMPARATIVA FINAL 4 PANELLS: FEBLE vs FORTA (VIC vs GARRIGA)
# ==============================================================================
library(tidyverse)
library(lubridate)
library(ggthemes)

# --- 1. DEFINICIÓ DE LA FORÇA DEL FENOMEN ---
# Clasificació dels dies segons la velocitat màxima a La Garriga (00h-11h)
mesos_hivern <- c(12, 1, 2)

filtre_forca <- meteo_dataset %>%
  filter(month(datetime) %in% mesos_hivern & saligarda == "si") %>%
  mutate(hora = hour(datetime), dia_mes = format(datetime, "%m-%d")) %>%
  filter(hora <= 11) %>%
  group_by(dia_mes) %>%
  summarise(mean_v = mean(suppressWarnings(as.numeric(hi_speed)), na.rm = TRUE)) %>%
  mutate(forca_cat = case_when(
    mean_v > 25 ~ "Saligarda FORTA (>35 km/h)",
    mean_v < 15 ~ "Saligarda FEBLE (<25 km/h)",
    TRUE ~ "Altres"
  )) %>%
  filter(forca_cat != "Altres")

# --- 2. PREPARACIÓ DE DADES PER LOCALITZACIÓ ---

# A) VIC (Dalt)
df_vic_4p <- vic_clean %>%
  filter(month(datetime) %in% mesos_hivern) %>%
  mutate(
    hora = hour(datetime),
    dia_mes = format(datetime, "%m-%d"),
    estacio = "VIC (Dalt)",
    vel = suppressWarnings(as.numeric(vic_wind)),
    dir = suppressWarnings(as.numeric(VAR_31))
  ) %>%
  inner_join(filtre_forca, by = "dia_mes") %>%
  select(forca_cat, hora, vel, dir, estacio)

# B) LA GARRIGA (Baix)
br_gar <- c("N"=0, "NNE"=22.5, "NE"=45, "ENE"=67.5, "E"=90, "ESE"=112.5, "SE"=135, "SSE"=157.5,
            "S"=180, "SSW"=202.5, "SW"=225, "WSW"=247.5, "W"=270, "WNW"=292.5, "NW"=315, "NNW"=337.5)

df_gar_4p <- meteo_dataset %>%
  filter(month(datetime) %in% mesos_hivern) %>%
  mutate(
    hora = hour(datetime),
    dia_mes = format(datetime, "%m-%d"),
    estacio = "LA GARRIGA (Baix)",
    vel = suppressWarnings(as.numeric(hi_speed)),
    dir = br_gar[toupper(trimws(as.character(hi_dir)))]
  ) %>%
  inner_join(filtre_forca, by = "dia_mes") %>%
  select(forca_cat, hora, vel, dir, estacio)

# --- 3. UNIÓ I CÀLCUL DE FLETXES INVERTIDES ---
full_comparativa <- bind_rows(df_vic_4p, df_gar_4p) %>%
  group_by(estacio, forca_cat, hora) %>%
  summarise(
    v_mean = mean(vel, na.rm = TRUE),
    v_sem  = sd(vel, na.rm = TRUE) / sqrt(n()),
    u = mean(sin(dir * pi/180), na.rm = TRUE),
    v = mean(cos(dir * pi/180), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # FLETXES INVERTIDES: Nord -> Cap avall (restem els components)
    xend = hora - (u * 0.7), 
    yend = v_mean - (v * 1.5)
  )

# Forcem l'ordre de les estacions (Vic a dalt)
full_comparativa$estacio <- factor(full_comparativa$estacio, 
                                   levels = c("VIC (Dalt)", "LA GARRIGA (Baix)"))

# --- 4. GRÀFIC DEFINITIU ---
ggplot(full_comparativa, aes(x = hora, y = v_mean, color = estacio)) +
  # Ombra d'error
  geom_ribbon(aes(ymin = pmax(0, v_mean - v_sem), ymax = v_mean + v_sem, fill = estacio), 
              alpha = 0.2, color = NA) +
  
  # Línia i punts
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  
  # Fletxes de direcció (Nord = Dalt)
  geom_segment(aes(xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.1, "cm")), color = "red", size = 0.6) +
  
  # MATRIU DE 4 PANELLS
  facet_wrap(~ estacio + forca_cat) +
  
  # Estètica i Escales
  scale_color_manual(values = c("VIC (Dalt)" = "#2980B9", "LA GARRIGA (Baix)" = "#E67E22")) +
  scale_fill_manual(values = c("VIC (Dalt)" = "#2980B9", "LA GARRIGA (Baix)" = "#E67E22")) +
  scale_x_continuous("Hora del dia", breaks = seq(0, 23, 4)) +
  scale_y_continuous("Velocitat mitjana (km/h)", limits = c(0, NA)) +
  theme_few() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines")
  ) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "grey85") +
  
  labs(title = "Comparativa del Perfil de Vent segons la intensitat de la Saligarda",
       subtitle = "4 panells: Estació (fila) vs Força del vent a la Garriga (columna). Nord = Dalt.",
       caption = "Nota: La força es defineix per la v. màxima registrada a La Garriga durant el matí.")

