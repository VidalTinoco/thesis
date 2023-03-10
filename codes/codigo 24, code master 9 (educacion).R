#### Tesis #####
##### Codigo 24: education plot#####

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
library (patchwork)

### setting the working directory
setwd ("C:/Users/vidal/Documents/Tesis")

###Cargando base de datos
ed <- read_csv("data/Final Data/df_ed_2005_mun.csv")
df <- read_csv("data/Final Data/df_final_general_year.csv")

summary(df)
summary(ed)


### revisamos que los nombres de los municipios coincidan
summary(ed$nom_mun %in% df$mun) ## 38 discrepancias:
ed$nom_mun[(ed$nom_mun %in% df$mun)==FALSE] ###son los que tienen acento

### quitamos acentos
ed_sa <- ed %>% 
  mutate(nom_mun = recode(nom_mun, 
                          "?lvaro Obreg?n" = "Alvaro Obregon",
                          "Apatzing?n" = "Apatzingan",
                          "Brise?as" = "Brisenas",
                          "Car?cuaro" = "Caracuaro",
                          "Coalcom?n de V?zquez Pallares" = "Coalcoman",
                          "Cop?ndaro" = "Copandaro",
                          "Cher?n" = "Cheran",
                          "Chuc?ndiro" = "Chucandiro",
                          "Erongar?cuaro" = "Erongaricuaro",
                          "Ixtl?n" = "Ixtlan",
                          "Jim?nez" = "Jimenez",
                          "Ju?rez" = "Juarez",
                          "Maravat?o" = "Maravatio",
                          "L?zaro C?rdenas" = "Lazaro Cardenas",
                          "M?gica" = "Mugica",
                          "Nocup?taro" = "Nocupetaro",
                          "Numar?n" = "Numaran",
                          "Pajacuar?n" = "Pajacuaran",
                          "Panind?cuaro" = "Panindicuaro",
                          "Par?cuaro" = "Paracuaro",
                          "P?tzcuaro" = "Patzcuaro",
                          "Perib?n" = "Periban",
                          "Pur?pero" = "Purepero",
                          "Puru?ndiro" = "Puruandiro",
                          "Quer?ndaro" = "Querendaro",
                          "Cojumatl?n de R?gules" = "Cojumatlan de Regules",
                          "Tac?mbaro" = "Tacambaro",
                          "Tanc?taro" = "Tancitaro",
                          "Tanganc?cuaro" = "Tangancicuaro",
                          "Tar?mbaro" = "Tarimbaro",
                          "Ting?ind?n" = "Tinguindin",
                          "Tiquicheo de Nicol?s Romero" = "Tiquicheo",
                          "Tumbiscat?o" = "Tumbiscatio",
                          "Yur?cuaro" = "Yurecuaro",
                          "Zin?paro" = "Zinaparo",
                          "Zinap?cuaro" = "Zinapecuaro",
                          "Zit?cuaro" = "Zitacuaro",
                          "Jos? Sixto Verduzco" = "Jose Sixto Verduzco"
                          ))
summary(ed_sa$nom_mun %in% df$mun) ## 0 discrepancias

### hacemos el join
summary(df)
summary(ed_sa)

df_ul <- left_join(df, ed_sa, by = c("mun" = "nom_mun"))
df_ul

### convirtiendo variables a factores
summary(df_ul)
df_ul <- df_ul %>% 
  mutate(mun = factor(mun))
summary(df_ul)

###Plot

##creamos otro df
plot_data <- df_ul %>%
  mutate(ed_t = ifelse(upper == 1, "Upper Education", "Lower Education"),
         ed_t = factor(ed_t),
         pto_f = ifelse(pto == 1, "(b) After 2011", " (a) Before 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(ed_t, pto_f) %>% 
  summarise(mean_gini = mean(gini_mean),
            se_gini = sd(gini_mean, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data)


## el plot
(dd_plot_a <- ggplot(plot_data, aes(x = pto_f, y = mean_gini, color = ed_t)) +
    #geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_point() +
    geom_line(aes(group = ed_t)) +
    labs(title = "Gini mean per education level",
         x = "Period", y = "Gini",
         color = "Group") + 
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    theme_clean()
)


ggsave("ddplot_ed.png", dd_plot_1, path = "graficas y mapas/Final", dpi = "retina")

### hacemos lo mismo pero para tratados y control
##hacemos nuevas df
dful_trat <- df_ul %>% 
  filter(trat_2 == 1)
summary(dful_trat)

dful_cont <- df_ul %>% 
  filter(trat_2 == 0)
summary(dful_cont)

## tratados

#creamos otro df
plot_data1 <- dful_trat %>%
  mutate(ed_t = ifelse(upper == 1, "Upper Education", "Lower Education"),
         ed_t = factor(ed_t),
         pto_f = ifelse(pto == 1, "(b) After 2011", " (a) Before 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(ed_t, pto_f) %>% 
  summarise(mean_gini = mean(gini_mean),
            se_gini = sd(gini_mean, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data1)


# el plot
(dd_plot_t <- ggplot(plot_data1, aes(x = pto_f, y = mean_gini, color = ed_t)) +
    #geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_point() +
    geom_line(aes(group = ed_t)) +
    labs(title = "Gini mean per education level",
         subtitle = "Treatment Group",
         x = "Period", y = "Gini",
         color = "Group") + 
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    theme_clean()
)


ggsave("ddplot_ed_trat.png", dd_plot_2, path = "graficas y mapas/Final/edu", dpi = "retina")

##control
#creamos otro df
plot_data2 <- dful_cont %>%
  mutate(ed_t = ifelse(upper == 1, "Upper Education", "Lower Education"),
         ed_t = factor(ed_t),
         pto_f = ifelse(pto == 1, "(b) After 2011", " (a) Before 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(ed_t, pto_f) %>% 
  summarise(mean_gini = mean(gini_mean),
            se_gini = sd(gini_mean, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data)


# el plot
(dd_plot_c <- ggplot(plot_data2, aes(x = pto_f, y = mean_gini, color = ed_t)) +
    #geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_point() +
    geom_line(aes(group = ed_t)) +
    labs(title = "Gini mean per education level",
         subtitle = "Control Group",
         x = "Period", y = "Gini",
         color = "Group") + 
    scale_color_manual(values = c("brown1", "cornflowerblue")) +
    theme_clean()
)


ggsave("ddplot_ed_control.png", dd_plot_3, path = "graficas y mapas/Final/edu", dpi = "retina")

###juntando plots
edudd_plots <- (dd_plot_a / dd_plot_t / dd_plot_c)

ggsave("edu_dd_plots_atc.png", edudd_plots, path = "graficas y mapas/Final/edu", dpi = "retina", width = 7, height = 7)
