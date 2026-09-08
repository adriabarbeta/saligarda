library(readxl)
library(stringr)
library(purrr)
library(lubridate)
library(ggthemes)
library(forcats)
library(tidyverse)

# --- Directori base ---
base_dir <- "D:/Saligarda/Dades/Dades"

# Crea dataset ------------------------------------------------------------


# Buscar tots els fitxers .xls
all_files <- list.files(
  base_dir,
  pattern = "\\.xls$|\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

# Filtrar subset (ajusta els anys o patrons segons vulguis)
files_subset <- all_files %>%
  .[str_detect(., "2015")] %>%
  .[str_detect(., "01")]  # per exemple, fitxers del dia 25

length(all_files)

# --- Funció principal ---
read_meteo_file <- function(file_path) {
  message("Llegint: ", basename(file_path))
  
  header_raw <- suppressMessages(read_excel(file_path, n_max = 2, col_names = FALSE))
  header <- header_raw %>%
    mutate_all(~ ifelse(is.na(.), "", .)) %>%
    apply(2, function(x) paste0(trimws(x), collapse = "_")) %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    tolower() %>%
    make.names(unique = TRUE)
  
  df <- suppressMessages(read_excel(file_path, skip = 2, col_names = FALSE, col_types = "text"))
  if (ncol(df) > length(header)) {
    extra_cols <- paste0("extra_col_", seq_len(ncol(df) - length(header)))
    header <- c(header, extra_cols)
  }
  names(df) <- header
  
  if (all(c("date","time") %in% names(df))) {
    df <- df %>%
      mutate(
        date = case_when(
          str_detect(date, "^[0-9]+\\.?[0-9]*$") ~ as.Date(as.numeric(date), origin = "1899-12-30"),
          str_detect(date, "^\\d{1,2}/\\d{1,2}/\\d{2,4}$") ~ suppressWarnings(as.Date(date, format="%d/%m/%Y")),
          TRUE ~ as.Date(NA)
        ),
        time_num = case_when(
          str_detect(time, "^[0-9]+\\.?[0-9]*$") ~ as.numeric(time) * 24,
          str_detect(time, "^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$") ~ {
            h <- as.numeric(str_extract(time, "^[0-9]{1,2}"))
            m <- as.numeric(str_extract(time, "(?<=:)\\d{2}"))
            h + m/60
          },
          TRUE ~ NA_real_
        )
      )
    
    # Crear datetime només amb valors vàlids
    df$datetime <- as.POSIXct(NA_real_, origin="1970-01-01", tz="UTC")
    valid <- which(!is.na(df$date) & !is.na(df$time_num))
    if (length(valid) > 0) {
      h <- as.integer(floor(df$time_num[valid]))
      m <- as.integer(round((df$time_num[valid] %% 1) * 60))
      df$datetime[valid] <- df$date[valid] + lubridate::hours(h) + lubridate::minutes(m)
    }
    
    df <- df %>%
      mutate(time_hours = time_num) %>%
      select(-time, -time_num)
  }
  
  # Columnes numèriques conegudes
  num_cols <- c("temp_out","hi_temp","low_temp","out_hum","hi_speed",
                "wind_chill","low_chill","bar","rain","rain_rate","rain_acum")
  for (col in intersect(num_cols,names(df))) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  
  df$file <- basename(file_path)
  df <- df %>% filter(!is.na(date))
  
  return(df)
}

# Llegir un fitxer individual
test <- read_meteo_file(files_subset[1])
head(test[, c("date","datetime","time_hours","hi_speed","hi_dir")])

# Llegir tots els fitxers amb gestió d’errors
meteo_dataset <- map_dfr(all_files, ~tryCatch(
  read_meteo_file(.x),
  error = function(e) { 
    message("❌ Error llegint fitxer: ", .x, " | ", e$message)
    NULL
  }
))

#write.csv(meteo_dataset,"D:/Saligarda/Dades/saligarda.csv")

# Mitjanes per temps ------------------------------------------------------
# 1️⃣ Llegim el fitxer

meteo_dataset <- read.csv("D:/Saligarda/Dades/saligarda.csv", stringsAsFactors = FALSE)

# 2️⃣ Convertim les dates i hores correctament
# Si tens una columna "date" i una altra "time", unim-les.
# Si només tens "datetime", la convertim directament.
if("datetime" %in% names(meteo_dataset)) {
  meteo_dataset <- meteo_dataset %>%
    mutate(datetime = ymd_hms(datetime))
} else if(all(c("date", "time") %in% names(meteo_dataset))) {
  meteo_dataset <- meteo_dataset %>%
    mutate(datetime = ymd_hms(paste(date, time)))
} else {
  stop("No hi ha cap columna 'datetime' ni parella 'date'+'time' al fitxer CSV.")
}

# 3️⃣ Extreiem components temporals
meteo_dataset <- meteo_dataset %>%
  mutate(
    date = as.Date(datetime),
    time_hours = hour(datetime) + minute(datetime)/60,
    time_halfhour = floor(time_hours * 2) / 2)

# 4️⃣ Funció per assignar estació
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

# 5️⃣ Direccions a graus
dir_angles <- c(
  N = 0, NNE = 22.5, NE = 45, ENE = 67.5,
  E = 90, ESE = 112.5, SE = 135, SSE = 157.5,
  S = 180, SSW = 202.5, SW = 225, WSW = 247.5,
  W = 270, WNW = 292.5, NW = 315, NNW = 337.5
)

# 6️⃣ Perfil per estació i mitja hora
profile_all <- meteo_dataset %>%
  group_by(season, time_halfhour) %>%
  summarise(
    hi_speed_mean = mean(hi_speed, na.rm = TRUE),
    hi_speed_sd   = sd(hi_speed, na.rm = TRUE),
    n_obs         = sum(!is.na(hi_speed)),
    hi_dir_mode   = names(sort(table(hi_dir), decreasing = TRUE))[1],
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

# 7️⃣ Gràfic amb error estàndard
ggplot(profile_all %>% filter(!season == "NA"), aes(x = time_halfhour, y = hi_speed_mean, color = season)) +
  geom_ribbon(aes(ymin = hi_speed_mean - hi_speed_sem, ymax = hi_speed_mean + hi_speed_sem, fill = season),
              alpha = 0.15, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.8) +
  geom_segment(aes(xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", alpha = 0.6) +
  facet_wrap(~season, ncol = 2) +
  theme_few() +
  theme(legend.position = "bottom") +
  labs(
    title = "Perfil mitjà de velocitat del vent per estació",
    x = "Hora del dia",
    y = "Velocitat mitjana (km/h)"
  )

# Filtrar matí i per estació

# Assegura't de tenir només mitjana de velocitat per hora i estació
profile_simple <- profile_all %>%
  group_by(season, time_halfhour) %>%
  summarise(
    hi_speed_mean = mean(hi_speed_mean, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(time_halfhour >= 4, time_halfhour <= 10) %>%
  arrange(season, time_halfhour)

inflexions <- profile_simple %>%
  group_by(season) %>%
  mutate(
  delta_speed = hi_speed_mean - lag(hi_speed_mean),
  prev_delta = lag(delta_speed)
  ) %>%
  filter(!is.na(delta_speed) & !is.na(prev_delta) & prev_delta > 0 & delta_speed < 0) %>%
  ungroup()
inflexions

first_inflexion <- inflexions %>%
  group_by(season) %>%
  slice_min(time_halfhour) %>%  # primer punt on delta passa de positiu a negatiu
  ungroup()

first_inflexion

max_morning <- profile_simple %>%
  filter(time_halfhour >= 4, time_halfhour <= 10) %>%
  group_by(season) %>%
  slice_max(hi_speed_mean, n = 1) %>%
  ungroup()

### Categories dia saligarda si o no

# Pas 1: filtrar només horari 0-10h i direccions N
meteo_matinada <- meteo_dataset %>%
  filter(time_hours >= 0 & time_hours <= 10) %>%
  mutate(nord_flag = hi_dir %in% c("N", "NE", "NNE") & hi_speed > 15)

# Pas 2: comptar hores amb condició per cada dia
saligarda_flag <- meteo_matinada %>%
  group_by(date) %>%
  summarise(
    hours_nord_vent = sum(nord_flag, na.rm = TRUE) * 0.5  # si cada fila és mitja hora
  ) %>%
  mutate(
    saligarda = ifelse(hours_nord_vent >= 5, "si", "no")
  )

# Pas 3: unir amb el dataset original si vols
meteo_dataset <- meteo_dataset %>%
  left_join(saligarda_flag %>% dplyr::select(date, saligarda), by = "date")

## Gràfic patrons

# --- Calcular perfil per mitja hora ---
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
    # Error estàndard
    hi_speed_sem = hi_speed_sd / sqrt(n_obs),
    # Coordenades fletxa
    angle_deg = dir_angles[hi_dir_mode],
    angle_rad = pi/180 * angle_deg,
    arrow_length = 1.5,
    xend = time_halfhour - arrow_length * sin(angle_rad),
    yend = hi_speed_mean - arrow_length * cos(angle_rad)
  )

# --- Gràfic ---
ggplot(profile_all %>% filter(!season == "NA"), aes(x = time_halfhour, y = hi_speed_mean)) +
  geom_ribbon(aes(ymin = hi_speed_mean - hi_speed_sem,
                  ymax = hi_speed_mean + hi_speed_sem),
              fill = "grey70", alpha = 0.3) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_segment(aes(xend = xend, yend = yend),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red") +
  facet_wrap(~season+saligarda,nrow = 4) +
  scale_x_continuous("Hora del dia", breaks = seq(0,24,2)) +
  scale_y_continuous("Mitjana velocitat vent (km/h)") +
  theme_few() +
  theme(legend.position = "bottom")+
  geom_hline(aes(yintercept=15))


#### Efecte dies saligarda

# --- 3️⃣ Resum de variables per dia ---
daily_summary <- meteo_dataset %>%
  group_by(date, saligarda, season) %>%
  summarise(
    hi_temp = mean(hi_temp, na.rm = TRUE),
    low_temp = mean(low_temp, na.rm = TRUE),
    out_hum = mean(out_hum, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4️⃣ Testos t de Student per comparar ---
compare_vars <- c("hi_temp", "low_temp", "out_hum")

t_results <- lapply(compare_vars, function(var) {
  ttest <- t.test(daily_summary[[var]] ~ daily_summary$saligarda)
  data.frame(
    variable = var,
    mean_saligarda = mean(daily_summary[[var]][daily_summary$saligarda == "si"], na.rm = TRUE),
    mean_no_saligarda = mean(daily_summary[[var]][daily_summary$saligarda == "no"], na.rm = TRUE),
    p_value = ttest$p.value
  )
}) %>% bind_rows()

print(t_results)

# --- 5️⃣ Gràfic comparatiu ---
daily_summary_long <- daily_summary %>% 
  tidyr::pivot_longer(cols = c(hi_temp, low_temp, out_hum),
                      names_to = "variable", values_to = "valor")

ggplot(daily_summary_long %>% filter(!saligarda == "NA"), 
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

# Anàlisi vent i temperatura (Claude) -------------------------------------


library(tidyverse)
library(lubridate)

# Filter for N-NE-NNE winds in early morning (midnight to 10am)
wind_analysis <- meteo_dataset %>%
  mutate(
    hour = hour(datetime),
    # Create categories for wind speed
    wind_category = case_when(
      hi_speed < 10 ~ "Low (0-10 km/h)",
      hi_speed < 20 ~ "Medium (10-20 km/h)",
      hi_speed >= 20 ~ "High (≥20 km/h)"
    ),
    # Bin wind speed for aggregation
    wind_bin = cut(hi_speed, breaks = seq(0, 60, by = 2), include.lowest = TRUE)
  ) %>%
  filter(
    hour >= 0 & hour < 10,  # Early morning: midnight to 10am
    hi_dir %in% c("N", "NE", "NNE")  # Northern-eastern winds
  )

# Summary statistics by season and wind speed category
summary_stats <- wind_analysis %>%
  group_by(season, wind_category) %>%
  summarise(
    n_obs = n(),
    mean_pressure = mean(bar, na.rm = TRUE),
    sd_pressure = sd(bar, na.rm = TRUE),
    min_pressure = min(bar, na.rm = TRUE),
    max_pressure = max(bar, na.rm = TRUE),
    mean_wind_speed = mean(hi_speed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(season, wind_category)

print("Summary Statistics by Season and Wind Speed:")
print(summary_stats)

# Aggregate data for plotting - much faster!
plot_data <- wind_analysis %>%
  group_by(season, wind_bin) %>%
  summarise(
    mean_pressure = mean(bar, na.rm = TRUE),
    sd_pressure = sd(bar, na.rm = TRUE),
    mean_wind = mean(hi_speed, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 10)  # Only include bins with enough data

# Visualization 1: Averaged Pressure vs Wind Speed by Season
p1 <- ggplot(plot_data, aes(x = mean_wind, y = mean_pressure, color = season, size = n)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(weight = n), method = "lm", se = TRUE, size = 1) +
  facet_wrap(~season) +
  labs(
    title = "Air Pressure vs Wind Speed (N-NE-NNE winds, 00:00-10:00)",
    subtitle = "Averaged in 2 km/h bins",
    x = "Wind Speed (km/h)",
    y = "Air Pressure (hPa)",
    color = "Season",
    size = "N observations"
  ) +
  theme_minimal()

print(p1)

# Correlation analysis by season
correlations <- wind_analysis %>%
  group_by(season) %>%
  summarise(
    correlation = cor(hi_speed, bar, use = "complete.obs"),
    p_value = cor.test(hi_speed, bar)$p.value,
    .groups = "drop"
  )

print("Correlation between Wind Speed and Pressure by Season:")
print(correlations)

# Linear models by season
print("\nLinear Models by Season:")
seasons <- unique(wind_analysis$season)
for (s in seasons) {
  cat(paste0("\n=== ", s, " ===\n"))
  season_data <- filter(wind_analysis, season == s)
  model <- lm(bar ~ hi_speed, data = season_data)
  print(summary(model))
}

# Calculate half-hour means by saligarda status and season
temp_profile <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%  # Exclude NAs
  mutate(
    hour_decimal = hour(datetime) + minute(datetime)/60
  ) %>%
  group_by(season, saligarda, time_halfhour) %>%
  summarise(
    mean_temp = mean(temp_out, na.rm = TRUE),
    sd_temp = sd(temp_out, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Summary statistics
summary_by_saligarda <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%
  group_by(season, saligarda) %>%
  summarise(
    n_days = n_distinct(date),
    n_obs = n(),
    mean_temp = mean(temp_out, na.rm = TRUE),
    sd_temp = sd(temp_out, na.rm = TRUE),
    min_temp = min(temp_out, na.rm = TRUE),
    max_temp = max(temp_out, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary Statistics by Season and Saligarda:")
print(summary_by_saligarda)

# Plot 1: Daily temperature profile by season
p1 <- ggplot(temp_profile, aes(x = time_halfhour, y = mean_temp, 
                               color = saligarda, linetype = saligarda)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_temp - sd_temp, 
                  ymax = mean_temp + sd_temp, 
                  fill = saligarda), 
              alpha = 0.2, color = NA) +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Daily Temperature Profile: Saligarda vs No Saligarda",
    subtitle = "Half-hour means with ±1 SD bands",
    x = "Hour of Day",
    y = "Temperature (°C)",
    color = "Saligarda",
    linetype = "Saligarda",
    fill = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Temperature difference (saligarda = si minus saligarda = no)
temp_diff <- temp_profile %>%
  select(season, time_halfhour, saligarda, mean_temp) %>%
  pivot_wider(names_from = saligarda, values_from = mean_temp) %>%
  mutate(temp_difference = si - no)

p2 <- ggplot(temp_diff, aes(x = time_halfhour, y = temp_difference)) +
  geom_line(size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Temperature Difference: Saligarda Days vs Non-Saligarda Days",
    subtitle = "Positive values = warmer during saligarda days",
    x = "Hour of Day",
    y = "Temperature Difference (°C)",
    caption = "Difference = Saligarda 'si' minus Saligarda 'no'"
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  theme_few()

print(p2)

# Statistical comparison by time period
temp_by_period <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%
  mutate(
    hour = hour(datetime),
    period = case_when(
      hour >= 0 & hour < 6 ~ "Night (00-06)",
      hour >= 6 & hour < 12 ~ "Morning (06-12)",
      hour >= 12 & hour < 18 ~ "Afternoon (12-18)",
      hour >= 18 & hour < 24 ~ "Evening (18-24)"
    )
  ) %>%
  group_by(season, saligarda, period) %>%
  summarise(
    mean_temp = mean(temp_out, na.rm = TRUE),
    sd_temp = sd(temp_out, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(season, period, saligarda)

print("\nMean Temperature by Time Period:")
print(temp_by_period)


# Humitat relativa --------------------------------------------------------

library(tidyverse)
library(lubridate)

# Calculate half-hour means by saligarda status and season
humidity_profile <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%  # Exclude NAs
  mutate(
    hour_decimal = hour(datetime) + minute(datetime)/60
  ) %>%
  group_by(season, saligarda, time_halfhour) %>%
  summarise(
    mean_hum = mean(out_hum, na.rm = TRUE),
    sd_hum = sd(out_hum, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Summary statistics
summary_by_saligarda <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%
  group_by(season, saligarda) %>%
  summarise(
    n_days = n_distinct(date),
    n_obs = n(),
    mean_hum = mean(out_hum, na.rm = TRUE),
    sd_hum = sd(out_hum, na.rm = TRUE),
    min_hum = min(out_hum, na.rm = TRUE),
    max_hum = max(out_hum, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary Statistics by Season and Saligarda:")
print(summary_by_saligarda)

# Plot 1: Daily humidity profile by season
p1 <- ggplot(humidity_profile, aes(x = time_halfhour, y = mean_hum, 
                                   color = saligarda, linetype = saligarda)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_hum - sd_hum, 
                  ymax = mean_hum + sd_hum, 
                  fill = saligarda), 
              alpha = 0.05, color = NA) +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Daily Humidity Profile: Saligarda vs No Saligarda",
    subtitle = "Half-hour means with ±1 SD bands",
    x = "Hour of Day",
    y = "Relative Humidity (%)",
    color = "Saligarda",
    linetype = "Saligarda",
    fill = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  theme_few() +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Humidity difference (saligarda = si minus saligarda = no)
humidity_diff <- humidity_profile %>%
  select(season, time_halfhour, saligarda, mean_hum) %>%
  pivot_wider(names_from = saligarda, values_from = mean_hum) %>%
  mutate(hum_difference = si - no)

p2 <- ggplot(humidity_diff, aes(x = time_halfhour, y = hum_difference)) +
  geom_line(size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Humidity Difference: Saligarda Days vs Non-Saligarda Days",
    subtitle = "Positive values = more humid during saligarda days",
    x = "Hour of Day",
    y = "Humidity Difference (%)",
    caption = "Difference = Saligarda 'si' minus Saligarda 'no'"
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  theme_minimal()

print(p2)

# Statistical comparison by time period
humidity_by_period <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%
  mutate(
    hour = hour(datetime),
    period = case_when(
      hour >= 0 & hour < 6 ~ "Night (00-06)",
      hour >= 6 & hour < 12 ~ "Morning (06-12)",
      hour >= 12 & hour < 18 ~ "Afternoon (12-18)",
      hour >= 18 & hour < 24 ~ "Evening (18-24)"
    )
  ) %>%
  group_by(season, saligarda, period) %>%
  summarise(
    mean_hum = mean(out_hum, na.rm = TRUE),
    sd_hum = sd(out_hum, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(season, period, saligarda)

print("\nMean Humidity by Time Period:")
print(humidity_by_period)


# Patrons saligarda -------------------------------------------------------

# Preparar dades: dies amb i sense saligarda
daily_analysis <- meteo_dataset %>%
  filter(!is.na(saligarda)) %>%
  mutate(
    hour = hour(datetime),
    is_saligarda_wind = hi_dir %in% c("N", "NE", "NNE")
  )

# 1. PATRÓ DEL VENT PER HORA
wind_hourly <- daily_analysis %>%
  group_by(season, saligarda, hour) %>%
  summarise(
    n_obs = n(),
    pct_N_wind = mean(is_saligarda_wind, na.rm = TRUE) * 100,
    mean_wind_speed = mean(hi_speed, na.rm = TRUE),
    .groups = "drop"
  )

# Trobar quan para el vent (només dies AMB saligarda)
wind_stop_time <- wind_hourly %>%
  filter(saligarda == "si", pct_N_wind < 50) %>%
  group_by(season) %>%
  slice_min(hour, n = 1) %>%
  select(season, hour_stops = hour, pct_wind = pct_N_wind)

print("=== HORA ON PARA EL VENT SALIGARDA (baixa del 50%) ===")
print(wind_stop_time)

# 2. PATRONS HORARIS: Temperatura
temp_hourly <- daily_analysis %>%
  group_by(season, saligarda, hour) %>%
  summarise(
    mean_temp = mean(temp_out, na.rm = TRUE),
    mean_low_temp = mean(low_temp, na.rm = TRUE),
    mean_hi_temp = mean(hi_temp, na.rm = TRUE),
    temp_range = mean(hi_temp - low_temp, na.rm = TRUE),
    .groups = "drop"
  )

# 3. PATRONS HORARIS: Humitat
humidity_hourly <- daily_analysis %>%
  group_by(season, saligarda, hour) %>%
  summarise(
    mean_humidity = mean(out_hum, na.rm = TRUE),
    sd_humidity = sd(out_hum, na.rm = TRUE),
    .groups = "drop"
  )

# 4. PATRONS HORARIS: Pressió
pressure_hourly <- daily_analysis %>%
  group_by(season, saligarda, hour) %>%
  summarise(
    mean_pressure = mean(bar, na.rm = TRUE),
    sd_pressure = sd(bar, na.rm = TRUE),
    .groups = "drop"
  )

# PLOT 1: Patró del vent N-NE-NNE
p1 <- ggplot(wind_hourly, aes(x = hour, y = pct_N_wind, 
                              color = saligarda, linetype = saligarda)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.5) +
  facet_wrap(~season) +
  labs(
    title = "Patró de Vent Catabàtic (N-NE-NNE): Dies AMB vs SENSE Saligarda",
    subtitle = "Percentatge d'observacions amb vent del N-NE-NNE",
    x = "Hora del Dia",
    y = "% Vent N-NE-NNE",
    color = "Saligarda",
    linetype = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# PLOT 2: Velocitat del vent
p2 <- ggplot(wind_hourly, aes(x = hour, y = mean_wind_speed, 
                              color = saligarda, linetype = saligarda)) +
  geom_line(size = 1.2) +
  geom_vline(data = wind_stop_time, aes(xintercept = hour_stops), 
             linetype = "dashed", color = "red", alpha = 0.3) +
  facet_wrap(~season) +
  labs(
    title = "Velocitat del Vent: Dies AMB vs SENSE Saligarda",
    subtitle = "Línia vermella = hora típica on para el vent saligarda",
    x = "Hora del Dia",
    y = "Velocitat Mitjana del Vent (km/h)",
    color = "Saligarda",
    linetype = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# PLOT 3: Temperatura (low, hi, mean)
temp_long <- temp_hourly %>%
  pivot_longer(cols = c(mean_low_temp, mean_hi_temp, mean_temp),
               names_to = "temp_type", values_to = "temperature") %>%
  mutate(temp_type = recode(temp_type,
                            "mean_temp" = "Mitjana",
                            "mean_low_temp" = "Mínima",
                            "mean_hi_temp" = "Màxima"))

p3 <- ggplot(temp_long, aes(x = hour, y = temperature, 
                            color = interaction(saligarda, temp_type),
                            linetype = saligarda)) +
  geom_line(size = 0.8) +
  geom_vline(data = wind_stop_time, aes(xintercept = hour_stops), 
             linetype = "dashed", color = "red", alpha = 0.3) +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Temperatura (Mín/Màx/Mitjana): Dies AMB vs SENSE Saligarda",
    subtitle = "Línia vermella = hora típica on para el vent saligarda",
    x = "Hora del Dia",
    y = "Temperatura (°C)",
    color = "Saligarda.Tipus",
    linetype = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# PLOT 4: Humitat relativa
p4 <- ggplot(humidity_hourly, aes(x = hour, y = mean_humidity, 
                                  color = saligarda, linetype = saligarda)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean_humidity - sd_humidity,
                  ymax = mean_humidity + sd_humidity,
                  fill = saligarda), alpha = 0.2, color = NA) +
  geom_vline(data = wind_stop_time, aes(xintercept = hour_stops), 
             linetype = "dashed", color = "red", alpha = 0.3) +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Humitat Relativa: Dies AMB vs SENSE Saligarda",
    subtitle = "Banda = ±1 SD. Línia vermella = hora típica on para el vent",
    x = "Hora del Dia",
    y = "Humitat Relativa (%)",
    color = "Saligarda",
    linetype = "Saligarda",
    fill = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4)

# PLOT 5: Pressió atmosfèrica
p5 <- ggplot(pressure_hourly, aes(x = hour, y = mean_pressure, 
                                  color = saligarda, linetype = saligarda)) +
  geom_line(size = 1.2) +
  geom_vline(data = wind_stop_time, aes(xintercept = hour_stops), 
             linetype = "dashed", color = "red", alpha = 0.3) +
  facet_wrap(~season, scales = "free_y") +
  labs(
    title = "Pressió Atmosfèrica: Dies AMB vs SENSE Saligarda",
    subtitle = "Condicions anticiclòniques en dies amb saligarda",
    x = "Hora del Dia",
    y = "Pressió (hPa)",
    color = "Saligarda",
    linetype = "Saligarda"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p5)

# ESTADÍSTIQUES RESUM: Nit i matí (00:00-12:00)
night_morning_stats <- daily_analysis %>%
  filter(hour >= 0 & hour < 12) %>%
  group_by(season, saligarda) %>%
  summarise(
    n_obs = n(),
    n_days = n_distinct(date),
    pct_N_wind = mean(is_saligarda_wind, na.rm = TRUE) * 100,
    mean_wind_speed = mean(hi_speed, na.rm = TRUE),
    mean_temp = mean(temp_out, na.rm = TRUE),
    mean_low_temp = mean(low_temp, na.rm = TRUE),
    mean_hi_temp = mean(hi_temp, na.rm = TRUE),
    mean_humidity = mean(out_hum, na.rm = TRUE),
    mean_pressure = mean(bar, na.rm = TRUE),
    .groups = "drop"
  )

print("\n=== RESUM NIT I MATÍ (00:00-12:00): AMB vs SENSE SALIGARDA ===")
print(night_morning_stats)

# CORRELACIONS durant nit/matí
correlations <- daily_analysis %>%
  filter(hour >= 0 & hour < 12) %>%
  group_by(season, saligarda) %>%
  summarise(
    cor_pressure_wind = cor(bar, hi_speed, use = "complete.obs"),
    cor_pressure_temp = cor(bar, temp_out, use = "complete.obs"),
    cor_temp_humidity = cor(temp_out, out_hum, use = "complete.obs"),
    .groups = "drop"
  )

print("\n=== CORRELACIONS NIT/MATÍ (00:00-12:00) ===")
print(correlations)

# Diferències absolutes entre dies AMB i SENSE saligarda
differences <- night_morning_stats %>%
  select(season, saligarda, mean_temp, mean_humidity, mean_pressure, pct_N_wind) %>%
  pivot_wider(names_from = saligarda, values_from = c(mean_temp, mean_humidity, mean_pressure, pct_N_wind)) %>%
  mutate(
    temp_diff = mean_temp_si - mean_temp_no,
    humidity_diff = mean_humidity_si - mean_humidity_no,
    pressure_diff = mean_pressure_si - mean_pressure_no,
    wind_diff = pct_N_wind_si - pct_N_wind_no
  ) %>%
  select(season, temp_diff, humidity_diff, pressure_diff, wind_diff)

print("\n=== DIFERÈNCIES (Saligarda SI - Saligarda NO) ===")
print(differences)