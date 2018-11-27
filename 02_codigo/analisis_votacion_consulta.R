### Paquetes ----
library(pacman)
p_load(ggrepel, scales, tidyverse, treemapify)

### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

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


## Votación total por municipio ----
voto_por_mpo <- 
  bd %>% 
  group_by(nom_mun) %>% 
  summarise(estado = last(nom_ent),
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


## Votación total por entidad ----
voto_por_edo <- 
  bd %>% 
  group_by(nom_ent) %>% 
  summarise(casilla = last(nom_casilla),
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


## Votación acumulada por estado y día ----
voto_acumulado_por_edo <- 
  bd %>% 
  group_by(estado, dia) %>%
  summarise(casilla = last(nom_casilla),
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
  group_by(estado) %>% 
  mutate(acumulado_p01_si = cumsum(suma_p01_si),
         acumulado_p01_no = cumsum(suma_p01_no),
         acumulado_p02_si = cumsum(suma_p02_si),
         acumulado_p02_no = cumsum(suma_p02_no),
         acumulado_p03_si = cumsum(suma_p03_si),
         acumulado_p03_no = cumsum(suma_p03_no),
         acumulado_p04_si = cumsum(suma_p04_si),
         acumulado_p04_no = cumsum(suma_p04_no),
         acumulado_p05_si = cumsum(suma_p05_si),
         acumulado_p05_no = cumsum(suma_p05_no),
         acumulado_p06_si = cumsum(suma_p06_si),
         acumulado_p06_no = cumsum(suma_p06_no),
         acumulado_p07_si = cumsum(suma_p07_si),
         acumulado_p07_no = cumsum(suma_p07_no),
         acumulado_p08_si = cumsum(suma_p08_si),
         acumulado_p08_no = cumsum(suma_p08_no),
         acumulado_p09_si = cumsum(suma_p09_si),
         acumulado_p09_no = cumsum(suma_p09_no),
         acumulado_p10_si = cumsum(suma_p10_si),
         acumulado_p10_no = cumsum(suma_p10_no)) %>% 
  ungroup()


### Gráfica: Número de votos registrados en cada estado de la república durante la consulta ----
voto_por_edo %>% 
  mutate(por_edo_p01 = round((suma_p01_total/sum(suma_p01_total))*100, 1)) %>%  
  ggplot() +
  geom_treemap(aes(area = suma_p01_total, fill = suma_p01_total), col = "white") +
  geom_treemap_text(aes(area = suma_p01_total, label = nom_ent), fontface = "bold", color = "white") +
  geom_treemap_text(aes(area = suma_p01_total, label = comma(suma_p01_total)), color = "white", padding.y = unit(8, "mm"), size = 16) +
  geom_treemap_text(aes(area = suma_p01_total, label = paste(por_edo_p01, "% del total", sep = "")), color = "white", padding.y = unit(14.5, "mm"), size = 15) +
  scale_fill_gradient(low = "grey80", high = "steelblue", guide = guide_colorbar(barwidth = 18, nbins = 6), labels = comma, breaks = pretty_breaks(n = 6)) +
  labs(title = str_to_upper(str_wrap("número de votos registrados en cada estado de la república durante la consulta de los 10 Programas Prioritarios", width = 80)),
       subtitle = str_wrap("El tamaño de cada rectángulo es proporcional al número de votos emitidos en el estado correspondiente. Mientras más grande y azul el recuadro, mayor el número de votos aportados por dicha entidad.", width = 150),
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide") +
  tema +
  theme(legend.position = "none")

ggsave(filename = "treemap_aportacion_votos_por_edo.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)

# Gráfica: núm. mpos en los que ganó la opción "NO" en alguna de las 10 preguntas incluidas en la consulta ----
voto_por_mpo %>% 
  summarise(gano_no_p01 = sum(suma_p01_no > suma_p01_si),
            gano_no_p02 = sum(suma_p02_no > suma_p02_si),
            gano_no_p03 = sum(suma_p03_no > suma_p03_si),
            gano_no_p04 = sum(suma_p04_no > suma_p04_si),
            gano_no_p05 = sum(suma_p05_no > suma_p05_si),
            gano_no_p06 = sum(suma_p06_no > suma_p06_si),
            gano_no_p07 = sum(suma_p07_no > suma_p07_si),
            gano_no_p08 = sum(suma_p08_no > suma_p08_si),
            gano_no_p09 = sum(suma_p09_no > suma_p09_si),
            gano_no_p10 = sum(suma_p10_no > suma_p10_si)) %>% 
  gather(key = pregunta,
         value = valor) %>% 
  mutate(pregunta = case_when(pregunta == "gano_no_p01" ~ "1. Construir el Tren Maya",
                              pregunta == "gano_no_p02" ~ "2. Construir el tren del Itsmo",
                              pregunta == "gano_no_p03" ~ "3. Construir la refinería en Dos Bocas",
                              pregunta == "gano_no_p04" ~ "4. Plantar árboles en 1 M de hectáres",
                              pregunta == "gano_no_p05" ~ "5. Duplicar pensión a adultos mayores de 68 años",
                              pregunta == "gano_no_p06" ~ "6. Otorgar becas y capacitación a 2.6 millones de jóvenes",
                              pregunta == "gano_no_p07" ~ "7. Becar a estudiantes de escuelas públicas de NMS",
                              pregunta == "gano_no_p08" ~ "8. Pensionar a 1 M de personas que viven con alguna discapacidad",
                              pregunta == "gano_no_p09" ~ "9. Garantizar atención médica y medicinas a población sin servicios de salud",
                              pregunta == "gano_no_p10" ~ "10. Proveer cobertura gratuita de internet"),
         pregunta = str_wrap(pregunta, width = 35), 
         pregunta = fct_rev(fct_relevel((pregunta), "10. Proveer cobertura gratuita de\ninternet", after = Inf)),
         valor_texto = ifelse(valor > 0, valor, "")) %>%
  ggplot(aes(x = pregunta, y = valor)) +
  geom_col(color = "steelblue") +
  geom_text(aes(label = valor), hjust = -1, size = 6, color = "salmon", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 560), breaks = c(seq(0, 500, 50), 548), labels = comma) +
  labs(title = str_wrap("NÚMERO DE MUNICIPIOS EN LOS QUE GANÓ LA OPCIÓN \"NO\" EN ALGUNA DE LAS 10 PREGUNTAS INCLUIDAS EN LA CONSULTA", width = 55),
       subtitle = str_wrap("La base de datos de México Decide incluye información de hasta 548 municipios", width = 150),
       x = NULL,
       y = "\nNúmero de municipos\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide", width = 120)) +
  tema

ggsave(filename = "municpios_gano_no.png", path = "03_graficas/", width = 15, height = 13, dpi = 100)


# Gráfica: núm. casillas en los que ganó la opción "NO" en alguna de las 10 preguntas incluidas en la consulta ---

voto_por_casilla %>% 
  summarise(gano_no_p01 = sum(suma_p01_no > suma_p01_si),
            gano_no_p02 = sum(suma_p02_no > suma_p02_si),
            gano_no_p03 = sum(suma_p03_no > suma_p03_si),
            gano_no_p04 = sum(suma_p04_no > suma_p04_si),
            gano_no_p05 = sum(suma_p05_no > suma_p05_si),
            gano_no_p06 = sum(suma_p06_no > suma_p06_si),
            gano_no_p07 = sum(suma_p07_no > suma_p07_si),
            gano_no_p08 = sum(suma_p08_no > suma_p08_si),
            gano_no_p09 = sum(suma_p09_no > suma_p09_si),
            gano_no_p10 = sum(suma_p10_no > suma_p10_si)) %>%
  gather(key = pregunta,
         value = valor) %>% 
  mutate(pregunta = case_when(pregunta == "gano_no_p01" ~ "1. Construir el Tren Maya",
                              pregunta == "gano_no_p02" ~ "2. Construir el tren del Itsmo",
                              pregunta == "gano_no_p03" ~ "3. Construir la refinería en Dos Bocas",
                              pregunta == "gano_no_p04" ~ "4. Plantar árboles en 1 M de hectáres",
                              pregunta == "gano_no_p05" ~ "5. Duplicar pensión a adultos mayores de 68 años",
                              pregunta == "gano_no_p06" ~ "6. Otorgar becas y capacitación a 2.6 millones de jóvenes",
                              pregunta == "gano_no_p07" ~ "7. Becar a estudiantes de escuelas públicas de NMS",
                              pregunta == "gano_no_p08" ~ "8. Pensionar a 1 M de personas que viven con alguna discapacidad",
                              pregunta == "gano_no_p09" ~ "9. Garantizar atención médica y medicinas a población sin servicios de salud",
                              pregunta == "gano_no_p10" ~ "10. Proveer cobertura gratuita de internet"),
         pregunta = str_wrap(pregunta, width = 35), 
         pregunta = fct_rev(fct_relevel((pregunta), "10. Proveer cobertura gratuita de\ninternet", after = Inf)),
         valor_texto = ifelse(valor > 0, valor, "")) %>%
  ggplot(aes(x = pregunta, y = valor)) +
  geom_col(color = "steelblue") +
  geom_text(aes(label = valor), hjust = -1, size = 6, color = "salmon", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1150), breaks = c(seq(0, 1000, 200), 1096), labels = comma) +
  labs(title = str_wrap("NÚMERO DE CASILLAS EN LAS QUE GANÓ LA OPCIÓN \"NO\" EN ALGUNA DE LAS 10 PREGUNTAS INCLUIDAS EN LA CONSULTA", width = 65),
       subtitle = str_wrap("La base de datos de México Decide incluye información de hasta 1,096 casillas", width = 150),
       x = NULL,
       y = "\nNúmero de casillas\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide", width = 120)) +
  tema

ggsave(filename = "casillas_gano_no.png", path = "03_graficas/", width = 15, height = 13, dpi = 100)  


### Gráfica: histograma votos por minuto por casilla ---

voto_por_casilla %>% 
  mutate(votos_por_minuto = round(suma_p01_total/(60*10*2), 1)) %>% 
  ggplot() +
  geom_histogram(aes(votos_por_minuto), bins = 60, fill = "steelblue", color = "white") +
  labs(title = str_wrap("DISTRIBUCIÓN DE LOS VOTOS POR MINUTO RECIBIDOS EN LAS CASILLAS DE LA CONSULTA DE LOS 10 PROGRAMAS PRIORITARIOS", width = 75),
       subtitle = str_wrap("Para este cálculo consideré que las casillas estuvieron en funcionamiento 1,200 minutos,* equivalentes a 60 minutos x 10 horas x 2 días", width = 150),
       x = "Votos por minuto\n",
       y = "\nNúmero de casillas\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide / *Nota: A diferencia de la consulta sobre el NAICM, para ésta no encontré algún documento que indicara el horario de funcionamiento de las casillas. Antes esto, asumí que era el mismo que en la primera consulta.", width = 150)) +
  tema

ggsave(filename = "histograma_votos_por_minuto_diario.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


### Gráfica: boxplot votos por minuto por casilla ---
voto_por_casilla %>% 
  mutate(votos_por_minuto = round(suma_p01_total/(60*10*2), 1)) %>% 
  ggplot() +
  geom_boxplot(aes(x = "", y = votos_por_minuto), outlier.colour = "salmon", color = "salmon", fill = "steelblue") +
  coord_flip() +
  labs(title = str_wrap("DISTRIBUCIÓN DE LOS VOTOS POR MINUTO RECIBIDOS EN LAS CASILLAS DE LA CONSULTA DE LOS 10 PROGRAMAS PRIORITARIOS", width = 75),
       subtitle = str_wrap("Para este cálculo consideré que las casillas estuvieron en funcionamiento 1,200 minutos,* equivalentes a 60 minutos x 10 horas x 2 días", width = 150),
       x = "Votos por minuto\n",
       y = "\nNúmero de casillas\n",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide / *Nota: A diferencia de la consulta sobre el NAICM, para ésta no encontré algún documento que indicara el horario de funcionamiento de las casillas. Antes esto, asumí que era el mismo que en la primera consulta.", width = 150)) +
  tema

ggsave(filename = "boxplot_votos_por_minuto_diario.png", path = "03_graficas/", width = 15, height = 10, dpi = 100)


### Gráfica: casillas con promedio de 3 o más votos por minuto ----

voto_por_casilla %>% 
  mutate(votos_por_minuto = round(suma_p01_total/(60*10*2), 1)) %>%
  select(estado, casilla, suma_p01_total, votos_por_minuto) %>% 
  filter(votos_por_minuto > 3) %>%    
  ggplot() +
  geom_col(aes(x = fct_reorder(str_to_title(casilla), votos_por_minuto), y = votos_por_minuto), fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12.5), breaks = seq(0, 12, 3)) +
  labs(title = str_to_upper(str_wrap("casillas en las que se registraron en promedio 3 o más votos por minuto, todos los minutos de la consulta", width = 55)),
       subtitle = str_wrap("Para este cálculo consideré que las casillas estuvieron en funcionamiento 1,200 minutos,* equivalentes a 60 minutos x 10 horas x 2 días.", width = 105),
       x = "\n",
       y = "\nPromedio de votos\npor minuto",
       caption = str_wrap("\nSebastián Garrido de Sierra / @segasi / Fuente: México Decide", width = 120)) +
  tema

ggsave(filename = "casillas_mas_tres_votos_por_minuto.png", path = "03_graficas/", width = 15, height = 10, dpi = 100) 