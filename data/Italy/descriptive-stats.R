library(tidyverse)
library(lubridate)
library(Hmisc)


# Percorso base GitHub per le tabelle giornaliere
url_path <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

# Specifica per la lettura colonne delle tabelle .csv
cols_specs <- cols(
  .default = col_double(),
  data = col_datetime(),
  stato = col_character(),
  codice_regione = col_character(),
  denominazione_regione = col_character(),
  note_it = col_character(),
  note_en = col_character()
)

# La specifica è ottenuta partendo dai guess fatti dal parser di readr
# Importa i dati totali una tantum

dati_regionali <- read_csv(url_path,col_types = cols_specs)
dati_regionali <- dati_regionali %>% rename("regione" = "denominazione_regione") %>% 
  group_by(regione) %>% 
  mutate(nuovi_deceduti      = deceduti - Lag(deceduti,1),
         nuovi_ospedalizzati = totale_ospedalizzati - Lag(totale_ospedalizzati,1),
         nuovi_terapia_int   = terapia_intensiva - Lag(terapia_intensiva,1),
         nuovi_tamponi       = tamponi - Lag(tamponi,1),
         nuovi_positivi      = totale_casi - Lag(totale_casi,1)) %>% 
  ungroup()


# 1: Grafici andamento regionale ----

theme_set(theme_bw())

dati_regionali %>% 
  ggplot(aes(x = data, y =  totale_casi)) +
  geom_point() + geom_line() + 
  scale_y_log10() + annotation_logticks(sides = "l") +
  facet_wrap(~ regione, ncol = 5) +
  labs(
    title = "Totale cumulato casi COVID-19", subtitle = "Per Regione o Provincia Autonoma",
    x = NULL, y = "Casi totali (scala logaritmica)"
  )


dati_regionali %>% 
  group_by(regione) %>% 
  arrange(regione, data) %>% 
  mutate(diff_decessi = c(NA, diff(deceduti))) %>% 
  ggplot(aes(x = data, y =  diff_decessi)) +
  geom_col() +
  scale_y_log10() + annotation_logticks(sides = "l") +
  facet_wrap(~ regione, ncol = 5 ) +
  labs(
    title = "Differenza giornaliere decessi COVID-19", subtitle = "Per Regione o Provincia Autonoma",
    x = NULL, y = "Differenza decessi (scala logaritmica)"
  )

## 1a: Aree Geografiche ----

nr_regioni_colpite <- 4 ## Seleziona numero di regioni colpite 
df.cov %>% 
  filter(data > "2020-03-03") %>% 
  group_by(denominazione_regione) %>% 
  summarise(tot_reg_lat = sum(totale_casi)) %>% 
  ungroup() %>%
  left_join(.,df.cov) %>% 
  select(denominazione_regione,lat,long,tot_reg_lat) %>% distinct() %>% 
  mutate(latitudine = if_else(lat > 44.44,"Nord",if_else(lat < 44.44 & lat > 41.2,"Centro","Sud"))) %>% 
  group_by(latitudine) %>% 
  top_n(n = nr_regioni_colpite,wt = tot_reg_lat) %>% 
  ungroup() %>% 
  left_join(.,df.cov) %>% 
  select(data,lat,totale_casi,denominazione_regione,tot_reg_lat,latitudine) -> df.temp

## Cambia ordine fattori e seleziona colori per numero regioni considerate
df.temp$latitudine  <- factor(df.temp$latitudine,levels = c("Nord","Centro","Sud"))
colourCount         <- length(unique(df.temp$denominazione_regione))
getPalette          <- colorRampPalette(brewer.pal(9, "Paired"))

ggplot(df.temp,aes(x = data,y = totale_casi,group = denominazione_regione))+
  geom_line(aes(color = denominazione_regione),size = 0.6)+
  geom_point(aes(color = denominazione_regione, shape = denominazione_regione))+
  facet_wrap(~latitudine)+
  geom_dl(aes(label = denominazione_regione,color = denominazione_regione), method="smart.grid", cex = 0.8) +
  theme_hc()+
  scale_y_continuous(expand = c(0,0),trans = "log")+
  scale_color_manual(values = getPalette(colourCount))+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  labs(
    title = "Curva Epidemica Per Regioni in Aree Geografiche", subtitle = "Scala logaritmica",
    x = NULL, y = "Totale Casi"
  )

rm(df.temp)
