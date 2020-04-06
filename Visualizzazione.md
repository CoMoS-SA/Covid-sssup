Visualizzazione dati COVID-19 in Italia
================

Progressione relativa
=====================

``` r
# Elenco ordinato di regioni da includere nel grafico
regioni_scelte <- c("Lombardia", "Veneto", "Piemonte", "Emilia-Romagna", "Toscana")

dati_regionali %>%
  mutate(regione_evidenziata  = if_else(regione %in% regioni_scelte, regione, NA_character_)) %>% 
  group_by(regione) %>% 
  filter(totale_casi >= 10) %>% 
  mutate(giorno_epidemia = 1:n()) %>% 
  ggplot(aes(x = giorno_epidemia, y = totale_casi, group= regione, color = regione_evidenziata)) +
  geom_point(data = . %>% filter(!is.na(regione_evidenziata)), size = 1) +
  geom_line() +
  scale_y_log10() + annotation_logticks(sides = "l") +
  labs(
    title = "Progressione casi totali COVID-19", subtitle = "Per Regione o Provincia Autonoma",
    x = "Giorni trascorsi dopo il decimo caso", y = "Casi totali (scala logaritmica)"
  )
```

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-2-1.png)

Grafici andamento regionale nel tempo
=====================================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-3-1.png)

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-4-1.png)

Incrementi Giornalieri per Regione
==================================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-5-1.png)

Curve di casi Totale per Regione
================================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-6-1.png)

Ripartizioni geografiche
========================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-7-1.png)

Tasso tamponi positivi
======================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-8-1.png)

Nuovi tamponi e nuovi positivi
==============================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-9-1.png)

Ospedali e terapia intensiva
============================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-10-1.png)

Stock e flow casi
=================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-11-1.png)

Distibuzione nuovi casi giornalieri (bunching?)
===============================================

![](Visualizzazione_files/figure-markdown_github/unnamed-chunk-12-1.png)
