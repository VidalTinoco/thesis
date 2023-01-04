#### Tesis #####
##### Codigo 20: code master 6 (est desc avo 2)#####

rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library (dplyr)
library (ggplot2)
library (PanelMatch)
library (readstata13)
library (readxl)
library (Synth)
library (tidyverse)
library (tidyr)
library (viridis)
library (ggthemes)
library (lubridate)
library (scales)
library (stargazer)
library (patchwork)

### setting the working directory
setwd ("C:/Users/vidal/Documents/Tesis")

###Cargando base de datos agricolas
ag <- read_xlsx("data/Datos Agricolas/datos_limpios_mich_ag.xlsx")

### explorando
summary(ag)

### seleccionamos variables que nos interesan
ag <- ag %>% 
  select(Anio,
         Nomestado,
         munsinac,
         Nomcultivo,
         Nomunidad,
         Cosechada) %>% 
  mutate(munsinac = factor(munsinac),
         Nomcultivo = factor(Nomcultivo),
         Nomunidad = factor(Nomunidad),
         Nomestado = factor(Nomestado)) %>% 
  filter(Nomcultivo == "Aguacate")
summary(ag)


### graficamos

##graf simple
(cosechada <- ggplot(data = ag %>% 
                    group_by(Anio) %>% 
                    summarise(cos_sum = sum(Cosechada, na.rm = TRUE)), 
                  aes(Anio, cos_sum)) +
    geom_point()
  
)

##poniendola wapa
(cosechada_1 <- ggplot(data = ag %>% 
                    group_by(Anio) %>% 
                    summarise(cos_sum = sum(Cosechada, na.rm = TRUE)),
                  aes(Anio, cos_sum)) +
    geom_point(colour = "seagreen") +
    geom_line(colour = "seagreen") +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=150000), angle=0, color = "red") +
    labs(title = "Harvested avocado",
         x = "Year", y = "Hectares harvested avocado", color = "Tratamiento",
         caption = "Source: SIAP") +
    theme_clean()
)

ggsave("avo_cosechado_0320.png", cosechada_1, path = "graficas y mapas/Final/Context", dpi = "retina")

