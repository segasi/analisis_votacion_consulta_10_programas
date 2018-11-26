### Paquetes ----
library(pacman)
p_load(ggrepel, scales, tidyverse)

### Cargar datos ----
bd <- read_csv("01_datos/resultado_mesas.csv")

### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 24, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))


### Calcular diversas métricas ----

## Votación total por casilla ----
voto_por_casilla <- 
  bd %>% 
  group_by(id_casilla) %>% 
  summarise(estado = last(nom_ent),
            municipio = last(nom_mun),
            casilla = last(nom_casilla),
            suma_p01_si = sum(p01_si, na.rm = T),
            suma_p01_no = sum(p01_no, na.rm = T),
            suma_p01_nulos = sum(p01_nulos, na.rm = T),
            suma_p01_total = sum(p01_total, na.rm = T),
            suma_p02_si = sum(p02_si, na.rm = T),
            suma_p02_no = sum(p02_no, na.rm = T),
            suma_p02_nulos = sum(p02_nulos, na.rm = T),
            suma_p02_total = sum(p02_total, na.rm = T),
            suma_p03_si = sum(p03_si, na.rm = T),
            suma_p03_no = sum(p03_no, na.rm = T),
            suma_p03_nulos = sum(p03_nulos, na.rm = T),
            suma_p03_total = sum(p03_total, na.rm = T),
            suma_p04_si = sum(p04_si, na.rm = T),
            suma_p04_no = sum(p04_no, na.rm = T),
            suma_p04_nulos = sum(p04_nulos, na.rm = T),
            suma_p04_total = sum(p04_total, na.rm = T),
            suma_p05_si = sum(p05_si, na.rm = T),
            suma_p05_no = sum(p05_no, na.rm = T),
            suma_p05_nulos = sum(p05_nulos, na.rm = T),
            suma_p05_total = sum(p05_total, na.rm = T),
            suma_p06_si = sum(p06_si, na.rm = T),
            suma_p06_no = sum(p06_no, na.rm = T),
            suma_p06_nulos = sum(p06_nulos, na.rm = T),
            suma_p06_total = sum(p06_total, na.rm = T),
            suma_p07_si = sum(p07_si, na.rm = T),
            suma_p07_no = sum(p07_no, na.rm = T),
            suma_p07_nulos = sum(p07_nulos, na.rm = T),
            suma_p07_total = sum(p07_total, na.rm = T),
            suma_p08_si = sum(p08_si, na.rm = T),
            suma_p08_no = sum(p08_no, na.rm = T),
            suma_p08_nulos = sum(p08_nulos, na.rm = T),
            suma_p08_total = sum(p08_total, na.rm = T),
            suma_p09_si = sum(p09_si, na.rm = T),
            suma_p09_no = sum(p09_no, na.rm = T),
            suma_p09_nulos = sum(p09_nulos, na.rm = T),
            suma_p09_total = sum(p09_total, na.rm = T),
            suma_p10_si = sum(p10_si, na.rm = T),
            suma_p10_no = sum(p10_no, na.rm = T),
            suma_p10_nulos = sum(p10_nulos, na.rm = T),
            suma_p10_total = sum(p10_total, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(por_p01_si = round((suma_p01_si/suma_p01_total)*100, 1),
         por_p01_no = round((suma_p01_no/suma_p01_total)*100, 1),
         por_p02_si = round((suma_p02_si/suma_p02_total)*100, 1),
         por_p02_no = round((suma_p02_no/suma_p02_total)*100, 1),
         por_p03_si = round((suma_p03_si/suma_p03_total)*100, 1),
         por_p03_no = round((suma_p03_no/suma_p03_total)*100, 1),
         por_p04_si = round((suma_p04_si/suma_p04_total)*100, 1),
         por_p04_no = round((suma_p04_no/suma_p04_total)*100, 1),
         por_p05_si = round((suma_p05_si/suma_p05_total)*100, 1),
         por_p05_no = round((suma_p05_no/suma_p05_total)*100, 1),
         por_p06_si = round((suma_p06_si/suma_p06_total)*100, 1),
         por_p06_no = round((suma_p06_no/suma_p06_total)*100, 1),
         por_p07_si = round((suma_p07_si/suma_p07_total)*100, 1),
         por_p07_no = round((suma_p07_no/suma_p07_total)*100, 1),
         por_p08_si = round((suma_p08_si/suma_p08_total)*100, 1),
         por_p08_no = round((suma_p08_no/suma_p08_total)*100, 1),
         por_p09_si = round((suma_p09_si/suma_p09_total)*100, 1),
         por_p09_no = round((suma_p09_no/suma_p09_total)*100, 1),
         por_p10_si = round((suma_p10_si/suma_p10_total)*100, 1),
         por_p10_no = round((suma_p10_no/suma_p10_total)*100, 1))
