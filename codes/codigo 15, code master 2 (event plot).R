#### Tesis #####
##### Codigo 15: Event Plot #####

rm (list=ls ()) # para vaciar el workspace donde vas a trabajar

### llamando a las librerias 
library (did)
library (dplyr)
library (ggplot2)
library (lfe)
library (PanelMatch)
library (readstata13)
library (readxl)
library (stargazer)
library (Synth)
library (tidyverse)
library (tidyr)
library (viridis)
library (ggthemes)
library (lubridate)
library (scales)
library (broom)
library (fixest)
library (ggiplot)



### setting the working directory
setwd ("C:/Users/vidal/Documents/Tesis")

###Cargando base de datos
df <- read_csv("data/Final Data/df_final_general_ginimonth.csv")

summary(df)

###Cambiando variables a factores
df <- df %>% mutate(cve = factor(cve),
                    mun = factor(mun),
                    mun_name = factor(mun_name),
                    mes = factor(mes),
                    pto = factor(pto))
summary(df)

###filtrando columnas que me importan
df <- df %>% 
  select(mun_name, 
         gini,
         mes,
         year,
         pto,
         trat_2,
         date)
summary(df)

###Creando variable time_to_treat
df <- df %>% 
  mutate(time_to_treat = ifelse(trat_2 == 1, year - 2011, 0))
summary(df)


### Event study models
mod_twfe = feols(gini ~ i(time_to_treat, trat_2, ref = -1)|mun_name + date,  cluster = ~mun_name, data = df)
summary(mod_twfe)



### Event Plot
ev_plot_agg <- ggiplot(mod_twfe, 
        aggr_eff = "post", aggr_eff.par = list(col = "seagreen"),
        main = 'Event Plot: Effect on Gini 
        (aggregated post- and/or pre-treatment effects)',
        xlab = 'Time to treatment',
        theme = theme_clean())

event_plot <- ggiplot(mod_twfe,
        main = 'Event Plot: Effect on Gini',
        xlab = 'Time to treatment',
        geom_style = 'ribbon',
        col = 'seagreen',
        theme = theme_clean())

event_plot_mix <- ggiplot(mod_twfe,
                          aggr_eff = "both", aggr_eff.par = list(col = "tomato1"),
                      main = 'Event Plot: Effect on Gini (TWFE)',
                      xlab = 'Time to treatment',
                      geom_style = 'ribbon',
                      col = 'seagreen',
                      theme = theme_clean())
event_plot_mix


ggsave("ev_plot_agg.png", ev_plot_agg, path = "graficas y mapas/Final/gini month", dpi = "retina")

ggsave("event_plot.png", event_plot, path = "graficas y mapas/Final/gini month", dpi = "retina")

ggsave("event_plot_mix.png", event_plot_mix, path = "graficas y mapas/Final/gini month", dpi = "retina")
