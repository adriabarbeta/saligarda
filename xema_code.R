### 
###
###
### Saligarda
library(tidyverse)
library(ggplot2)
library(lubridate)

temps <- read.csv("D:/Saligarda/Dades_meteorol_giques_de_la_XEMA.csv")

### Filtrem dades

filtrat <- temps %>%
  filter(CODI_VARIABLE==30|CODI_VARIABLE==31|CODI_VARIABLE==32|CODI_VARIABLE==33|
           CODI_VARIABLE==34|CODI_VARIABLE==35|CODI_VARIABLE==36|
           CODI_VARIABLE==46|CODI_VARIABLE==47|CODI_VARIABLE==48|CODI_VARIABLE==49) %>%
  mutate(Date=lubridate::dmy_hms(DATA_LECTURA)) %>%
  mutate(Hour=hour(Date),Month=month(Date),Year=year(Date),doy=lubridate::yday(Date)) %>%
  filter(CODI_ESTAT=="V"|CODI_BASE=="HO")

head(filtrat)

filtrat <- filtrat %>%
          dplyr::select(-c("ID","DATA_LECTURA","CODI_ESTAT","CODI_BASE")) %>%
          pivot_wider(id_cols = c("CODI_ESTACIO","Date","Hour","Month","Year","doy"),
                      names_from = "CODI_VARIABLE",values_from = "VALOR_LECTURA") 
head(filtrat)

names(filtrat) <- c("CODI_ESTACIO","Date","Hour","Month","Year","doy",
                    "Temp","RelHum","Precip","Rad","Pressure","WindSpeed2","WindDir2","WindDir10","WindSpeed10")

vent <- filtrat %>% filter(!WindSpeed2=="NA")

# Agreguem dades per estacio i dies ---------------------------------------

daily <- filtrat %>%
            dplyr::select(-c("Hour")) %>%
          group_by(CODI_ESTACIO,doy,Year) %>%
          dplyr::summarise(WS2_mean=mean(WindSpeed2,na.rm=T),WS10_mean=mean(WindSpeed10,na.rm=T))

taga <- daily %>% filter(CODI_ESTACIO=="VX")

torrem <- daily %>% filter(CODI_ESTACIO=="X9")

ggplot(torrem,aes(x=Year,doy,y=WS10_mean))+
  geom_point()+
  geom_smooth()
