library(tidyverse)
library(lubridate)
library(directlabels)
library(ggthemes)
library(scales)
library(tibbletime)


# Ripartizioni geografiche ----
# Ripartizione a 5: Nord-ovest, Nord-est, Centro, Sud, Isole (definizione Istat)
# Ripartizione a 3: Nord, Centro, Sud
rip_regioni <- read_csv("ripartizione_regioni.csv", col_types = "ccc") %>% 
  mutate(
    ripartizione_3 = factor(ripartizione_3) %>% fct_inorder(),
    ripartizione_5 = factor(ripartizione_5) %>% fct_inorder()
  )

# Scaricare tabella riepilogativa dati regionali ----
dati_regionali <- read_csv(
  # Percorso tabella riepilogativa, aggiornata giornalmente
  file = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
  # Schema tabella: specifica tipo di dato colonne;
  # (La specifica non è strettamente necessaria, ma rende l'importazione della tabella più robusta a errori)
  col_types = cols(
    .default = col_double(),
    data = col_datetime(),
    stato = col_character(),
    codice_regione = col_character(),
    denominazione_regione = col_character(),
    note_it = col_character(),
    note_en = col_character()
  )
) %>%
  rename("regione" = "denominazione_regione") %>%
  # Aggungere ripartizioni delle regioni
  left_join(rip_regioni, by = "regione") %>% 
  # Data come giorno, rimuovendo ora del giorno
  mutate(data = as.Date(data))



# Calcolo incremento giornalieri
dati_regionali <- dati_regionali %>%
  group_by(regione) %>%
  mutate(
    nuovi_deceduti = deceduti - lag(deceduti, 1),
    nuovi_ospedalizzati = totale_ospedalizzati - lag(totale_ospedalizzati, 1),
    nuovi_terapia_int = terapia_intensiva - lag(terapia_intensiva, 1),
    nuovi_tamponi = tamponi - lag(tamponi, 1),
    nuovi_positivi = totale_casi - lag(totale_casi, 1)
  ) %>%
  ungroup()


# 1: Grafici andamento regionale ----

theme_set(theme_bw())

dati_regionali %>%
  ggplot(aes(x = data, y = totale_casi)) +
  geom_point() +
  geom_line() +
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
  ggplot(aes(x = data, y = diff_decessi)) +
  geom_col() +
  scale_y_log10() + 
  annotation_logticks(sides = "l") +
  facet_wrap(~regione, ncol = 5) +
  labs(
    title = "Differenza giornaliere decessi COVID-19", subtitle = "Per Regione o Provincia Autonoma",
    x = NULL, y = "Differenza decessi (scala logaritmica)"
  )


## 1a: Aree Geografiche ----

nr_regioni_colpite <- 4 ## Seleziona numero di regioni colpite


# Seleziona regioni più colpite
dati_regioni_colpite <- dati_regionali %>%
  # nella data più recente
  filter(data == max(data)) %>%
  # raggruppa regioni per ogni ripartizione
  group_by(ripartizione_3) %>%
  # seleziona N regioni più colpite sulla base di totale_casi
  top_n(n = nr_regioni_colpite, wt = totale_casi) %>% 
  # assegna numeri progressivi alle regioni, 
  # ripetuti tra ripartizioni diverse, per colori e forme
  ungroup %>% arrange(ripartizione_3, desc(totale_casi)) %>% 
  mutate(reg_colpita = 1:n() %>% factor()) %>% 
  # Seleziona i dati per solo queste regioni
  select(regione, reg_colpita) %>% 
  left_join(dati_regionali, by = "regione") 


ggplot(dati_regioni_colpite, aes(x = data, y = totale_casi, group = regione, label = regione, color = reg_colpita, shape = reg_colpita)) +
  geom_line(size = 0.6) +
  geom_point() +
  facet_grid(~ ripartizione_3) +
  geom_dl(method = "smart.grid", cex = 0.8) +
  scale_y_log10() + annotation_logticks(sides = "l") +
  scale_x_date(breaks = "1 week", date_labels = "%d %b") +
  scale_color_manual(values = brewer.pal(n = nr_regioni_colpite, 'Set2') %>% rep(3)) +
  scale_shape_manual(values = c(0, 1, 2, 4) %>% rep(3)) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
    ) + 
  labs(
    title = "Curva Epidemica Per Regioni in Aree Geografiche", subtitle = "Scala logaritmica",
    x = NULL, y = "Totale Casi"
  )

rm(df.temp)

# Tasso tamponi positivi ----

# Definisce funzione per calcolo media mobile su due giorni
rolling_mean <- rollify(mean, window = 2)

dati_regionali %>% 
  group_by(regione) %>% 
  filter(regione %in% c("Lombardia", "Veneto", "Emilia-Romagna", "Toscana", "Lazio", "Piemonte", "Campania")) %>% 
  mutate(tasso_positivi = rolling_mean(totale_positivi / tamponi)) %>%
  ggplot(aes(x = data, y = tasso_positivi, color = regione, label = regione)) + 
  geom_dl(method = "last.qp") +
  geom_point() + geom_line() + 
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  scale_x_date(breaks = "1 week", date_labels = "%d %b", limits = c(ymd(2020-02-28), max(dati_regionali$data) + days(3))) +
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "Tasso di test tampone positivi",
    y = "Tamponi positivi su tamponi effettuati", x = NULL,
    subtitle = "Media mobile di 2 giorni"
  )
  

# Nuovi tamponi e nuovi positivi ----

dati_regionali %>% 
  filter(regione %in% c("Lombardia", "Veneto", "Piemonte", "Emilia-Romagna", "Toscana", "Lazio")) %>% 
  mutate(regione = fct_relevel(regione, "Lombardia", "Veneto", "Piemonte", "Emilia-Romagna", "Toscana", "Lazio")) %>% 
  select(data, regione, nuovi_tamponi, nuovi_positivi) %>% 
  pivot_longer(cols = nuovi_tamponi:nuovi_positivi) %>% 
  ggplot(aes(x = data, y = value, fill = name)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~regione) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(breaks = "1 week", date_labels = "%d %b", limits = c(ymd(2020-02-28), max(dati_regionali$data) + days(3))) +
  scale_y_continuous(labels = scales::comma) + 
  theme(legend.position = "top") +
  labs(
    title = "Tamponi effettuati e positivi",
    x = NULL,
    subtitle = "Incremento giornaliero"
  )
