#### Tesis #####
##### Codigo 17: code master 1 (est desc avo)#####

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
         Volumenproduccion,
         Precio,
         Valorproduccion) %>% 
  mutate(munsinac = factor(munsinac),
         Nomcultivo = factor(Nomcultivo),
         Nomunidad = factor(Nomunidad),
         Nomestado = factor(Nomestado))
summary(ag)


### Vemos top 3 valprod de cultivo de 03 a 20

## sumamos por cultivo nada mas
(ag_plus <- ag  %>% 
    group_by(Nomcultivo) %>%
    mutate(sum_all_cultivo_valpod = sum(Valorproduccion)) %>% 
    ungroup()
    
  
)
summary(ag_plus)

## exploramos
sort(unique(ag_plus$Nomcultivo)) #165
sort(unique(ag_plus$sum_all_cultivo_valpod)) 

##obtenemos top valores de cada variable de interes
(agplus_topvalp <- ag_plus %>% 
  group_by(Nomcultivo) %>% 
  slice_max(sum_all_cultivo_valpod, n = 1, with_ties = FALSE) %>%
  arrange(desc(sum_all_cultivo_valpod)) %>%
  head(10)
) #Aguacate, maiz grano, zarzamora
#328465613808 ##Check

##Obtenemos informacion para tabla
tabtop_valp_cul_0320 <- agplus_topvalp %>% 
  select(Nomcultivo, Nomunidad, sum_all_cultivo_valpod)
tabtop_valp_cul_0320

##Tabla
stargazer(as.data.frame(tabtop_valp_cul_0320)
          , type="text") ###FALTA



### Graficamos
(topvalp_0320 <- ggplot(data = ag %>% 
                          filter(Nomcultivo == "Aguacate"|
                                   Nomcultivo == "Maíz grano"|
                                   Nomcultivo == "Zarzamora") %>% 
                          group_by(Anio, Nomcultivo) %>% 
                          summarise(valp_sum = sum(Valorproduccion, na.rm = TRUE)), 
                        aes(Anio, valp_sum, color = Nomcultivo)) +
    geom_point()
  
)

##poniendola wapa
(topvalp_0320 <- ggplot(data = ag %>% 
                          filter(Nomcultivo == "Aguacate"|
                                   Nomcultivo == "Maíz grano"|
                                   Nomcultivo == "Zarzamora") %>% 
                          group_by(Anio, Nomcultivo) %>% 
                          summarise(valp_sum = sum(Valorproduccion, na.rm = TRUE)), 
                        aes(Anio, valp_sum, color = Nomcultivo)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=28000000000), angle=0, color = "red") +
    labs(title = "Crop Production Value",
         x = "Year", y = "Value (pesos)", color = "Crop",
         caption = "Source: SIAP") +
    scale_color_manual(labels = c("Avocado", "Grain Corn", "Blackberry"), values = c("seagreen", "gold2", "darkorchid1")) +
    theme_clean()
)

ggsave("valorprod_top3crops.png", topvalp_0320, path = "graficas y mapas/Final/Context", dpi = "retina")


##exploramos como le fue al aguacate
avo <- ag %>% 
  filter(Nomcultivo == 'Aguacate')
summary(avo)

### graficamos

##graf simple
(avo_ag <- ggplot(data = avo %>% 
                          group_by(Anio) %>% 
                          summarise(valp_sum = sum(Valorproduccion, na.rm = TRUE)), 
                        aes(Anio, valp_sum)) +
    geom_point()
  
)

##poniendola wapa
(avo_ag <- ggplot(data = avo %>% 
                    group_by(Anio) %>% 
                    summarise(valp_sum = sum(Valorproduccion, na.rm = TRUE)),
                  aes(Anio, valp_sum)) +
    geom_point(colour = "seagreen") +
    geom_line(colour = "seagreen") +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=28000000000), angle=0, color = "red") +
    labs(title = "Avocado Production Value",
         x = "Year", y = "Production Value (pesos)", color = "Tratamiento",
         caption = "Source: SIAP") +
    theme_clean()
)

(avo_ag1 <- ggplot(data = avo %>% 
                    group_by(Anio) %>% 
                    summarise(volp_sum = sum(Volumenproduccion, na.rm = TRUE)),
                  aes(Anio, volp_sum)) +
    geom_point(colour = "seagreen") +
    geom_line(colour = "seagreen") +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=1500000), angle=0, color = "red") +
    labs(title = "Avocado Production Volume",
         x = "Year", y = "Production Volume (tons)",
         caption = "Source: SIAP") +
    theme_clean()
)

(avo_ag2 <- ggplot(data = avo %>% 
                     group_by(Anio) %>% 
                     summarise(price_mean = mean(Precio, na.rm = TRUE)),
                   aes(Anio, price_mean)) +
    geom_point(colour = "seagreen") +
    geom_line(colour = "seagreen") +
    geom_vline(xintercept= 2011, linetype=2, show.legend = TRUE) +
    geom_text(aes(x = 2012, label="2011", y=17000), angle=0, color = "red") +
    labs(title = "Avocado Mean Price per Year",
         x = "Year", y = "Avocado Price (pesos per ton)",
         caption = "Source: SIAP") +
    theme_clean()
)

### Juntando plots de aguacate
avo <- (avo_ag / avo_ag1 / avo_ag2)

ggsave("context_avo_0320.png", avo, path = "graficas y mapas/Final/Context", dpi = "retina", width = 7, height = 7)






