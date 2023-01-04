#### Tesis #####
##### Codigo 14: code master 1 (regresiones y est desc)#####

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
                    trat_2 = factor(trat_2),
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


### Comparando tendencias: Aguacateros vs No Aguacateros
##Grafica sin embellecer
(ag0 <- ggplot (
  data=df %>% group_by(trat_2, date) %>% summarise(gini_mean = mean (gini, na.rm=T))
  , aes(x=date, y=gini_mean, color=trat_2))
  + scale_x_date(labels = date_format("%Y-%m-%d"))
  + geom_vline(xintercept = ymd(20110101), linetype = 2)
  + geom_line()
) 

##Embelleciendo
(ag <- ggplot (
  data=df %>% group_by(trat_2, date) %>% summarise(gini_mean = mean (gini, na.rm=T))
  , aes(x=date, y=gini_mean, color=trat_2))
  + scale_x_date(labels = date_format("%Y-%m-%d"))
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_line()
  + geom_text(aes(x = ymd(20111201), label="2011", y=0.325), angle=0, color = "red")
  + labs(title = "Gini por Municipios Aguacateros y No Aguacateros",
         x = "Time", y = "Gini", color = "Tratamiento",
         caption = "Elaboracion propia con datos del IMSS y el SIAP
         [Si es municipio aguacatero desde 2003, entonces siempre tratamiento, 
         de otro modo municipio de control]")
  + scale_color_manual(labels = c("No Aguacateros", "Aguacateros"), values = c("tan3", "seagreen")) +
    theme_clean()
) 

ggsave("plot_gini_month_trat.png", ag, path = "graficas y mapas/Final", dpi = "retina")


### identificando el valor de Y en los 4 grupos
(y_treat_before = mean(filter(df, trat_2 == 1 & pto == 0)$gini, na.rm = T))
(y_treat_after = mean(filter(df, trat_2 == 1 & pto == 1)$gini, na.rm = T))
(y_control_before = mean(filter(df, trat_2 == 0, pto == 0)$gini, na.rm = T))
(y_control_after = mean(filter(df, trat_2 == 0, pto == 1)$gini, na.rm = T))

### la diferencia en diferencias es:
(y_treat_after - y_control_after) - (y_treat_before - y_control_before)  #0.003418386



###corriendo regresiones
summary(mod1 <- felm(gini ~ trat_2 + pto + (trat_2*pto) | 0 | 0 | 0, data=df))

###Two-Way Fixed-Effects (TWFE) y errores agrupados por municipio
summary(mod2 <- felm (gini ~ (trat_2*pto) | mun_name + date | 0 | mun_name, data=df))

###Tablas
stargazer(mod1, mod2
          , type="html", out="tablas/Final/PRUEBAtabla1_regresiones_mes_gini_mun_ptotrat.html")

##Embelleciendo
stargazer(mod1, mod2
          , title = "Tabla 1. Efectos del cultivo del aguacate en la desigualdad"
          , omit.stat=c ("f", "ser", "aic", "bic", "ll")
          , dep.var.caption = ""
          , covariate.labels = c("Municipio Aguacatero", "2011", "Municipio Aguacatero * 2011")
          , dep.var.labels = c("Gini") 
          , type="html", out="tablas/Final/tabla1_regresiones_mes_gini_mun_ptotrat.html")


#### Estadistica descriptiva ####
summary(df)

### mapa de densidad
#2003 a 2020
(ggplot(df, aes(gini, ..scaled..)) +
    geom_density() +
    facet_wrap(~trat_2)
)

## embelleciendo
(dens_tog <- ggplot(df, aes(gini, ..scaled.., colour = trat_2)) +
    geom_density(alpha = 0.2, na.rm = T) +
    labs(title = "Gini distribution per group (treatment and control)",
         x = "Gini", y = "Percent",
         caption = "2003-2020",
         colour = "Group") +
    scale_colour_manual(labels = c("No Aguacateros", "Aguacateros"), values = c("tan3", "seagreen")) +
    theme_clean()
) 

ggsave("dens_03_20_01.png", dens_tog, path = "graficas y mapas/Final", dpi = "retina")


#2003 a 2010
df_03_10 <- df %>% filter(year < 2011)

(dens_tog_0310 <- ggplot(df_03_10, aes(gini, ..scaled.., colour = trat_2)) +
    geom_density(alpha = 0.2, na.rm = T) +
    labs(title = "Gini distribution per group (treatment and control)",
         x = "Gini", y = "Percent",
         caption = "2003-2010",
         colour = "Group") +
    scale_colour_manual(labels = c("No Aguacateros", "Aguacateros"), values = c("tan3", "seagreen")) +
    theme_clean()
) 

ggsave("dens_03_10_01.png", dens_tog_0310, path = "graficas y mapas/Final", dpi = "retina")

#2011 a 2020
df_11_20 <- df %>% filter(year >= 2011)

(dens_tog_1120 <- ggplot(df_11_20, aes(gini, ..scaled.., colour = trat_2)) +
    geom_density(alpha = 0.2, na.rm = T) +
    labs(title = "Gini distribution per group (treatment and control)",
         x = "Gini", y = "Percent",
         caption = "2011-2020",
         colour = "Group") +
    scale_colour_manual(labels = c("No Aguacateros", "Aguacateros"), values = c("tan3", "seagreen")) +
    theme_clean()
) 

ggsave("dens_11_20_01.png", dens_tog_1120, path = "graficas y mapas/Final", dpi = "retina")


## DD Plot

#Creando nuevo df
plot_data <- df %>%
  mutate(trat_2_f = ifelse(trat_2 == "1", "Aguacatero", "No Aguacatero"),
         trat_2_f = factor(trat_2_f),
         pto_f = ifelse(pto == "1", "(b) After 2011", " (a) Before 2011"),
         pto_f = factor(pto_f)) %>% 
  group_by(trat_2_f, pto_f) %>% 
  summarise(mean_gini = mean(gini),
            se_gini = sd(gini, na.rm = T) / sqrt(n()),
            upper = mean_gini + (-1.96 * se_gini),
            lower = mean_gini + (1.96 * se_gini))

summary(plot_data)

(dd_plot_1 <- ggplot(plot_data, aes(x = pto_f, y = mean_gini, color = trat_2_f)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
    geom_line(aes(group = trat_2_f)) +
    labs(title = "Gini mean per group and period",
         x = "Period", y = "Gini",
         color = "Group") + 
    scale_color_manual(values = c("seagreen", "tan3")) +
    theme_clean()
)

ggsave("ddplot_month.png", dd_plot_1, path = "graficas y mapas/Final", dpi = "retina")


### Otra manera DD format (a mano)
summary(df)
df <- df %>% mutate(pto = factor(pto))

df_dd <- df %>% 
  group_by(pto, trat_2) %>% 
  summarise(mean_gini = mean(gini))
df_dd

## Jalamos valores y hacemos las restas para sacar el dd estimate
before_treatment <- df_dd %>% 
  filter(pto == 0, trat_2 == 1) %>% 
  pull(mean_gini)

before_control <- df_dd %>% 
  filter(pto == 0, trat_2 == 0) %>% 
  pull(mean_gini)

after_treatment <- df_dd %>% 
  filter(pto == 1, trat_2 == 1) %>% 
  pull(mean_gini)

after_control <- df_dd %>% 
  filter(pto == 1, trat_2 == 0) %>% 
  pull(mean_gini)

diff_treatment_before_after <- after_treatment - before_treatment
diff_control_before_after <- after_control - before_control
diff_diff <- diff_treatment_before_after - diff_control_before_after

diff_before_treatment_control <- before_treatment - before_control
diff_after_treatment_control <- after_treatment - after_control
other_diff_diff <- diff_after_treatment_control - diff_before_treatment_control

other_diff_diff ### 0.003418386 (salio igual que el anterior)

## Hacemos el plot
(dd_plot_2 <- ggplot(df_dd, aes(x = factor(pto), 
                                y = mean_gini, 
                                color = factor(trat_2))) + 
    geom_point() +
    geom_line(aes(group = factor(trat_2))) +
    annotate(geom = "segment", x = "0", xend = "1",
             y = before_treatment, yend = after_treatment - diff_diff,
             linetype = "dashed", color = "grey50") +
    annotate(geom = "segment", x = "1", xend = "1",
             y = after_treatment, yend = after_treatment - diff_diff,
             linetype = "dotted", color = "blue") +
    annotate(geom = "label", x = 2.15, y = after_treatment - (diff_diff / 2), 
             label = "Program effect", size = 1.5) +
    theme_clean()
)

#Embelleciendo
summary(df_dd)
(dd_plot_2 <- ggplot(df_dd, aes(x = factor(pto), 
                                y = mean_gini, 
                                color = factor(trat_2))) + 
    geom_point() +
    geom_line(aes(group = factor(trat_2))) +
    annotate(geom = "segment", x = "0", xend = "1",
             y = before_treatment, yend = after_treatment - diff_diff,
             linetype = "dashed", color = "grey50") +
    annotate(geom = "segment", x = "1", xend = "1",
             y = after_treatment, yend = after_treatment - diff_diff,
             linetype = "dotted", color = "blue") +
    annotate(geom = "label", x = 2.2, y = after_treatment - (diff_diff / 2), 
             label = "Program effect", size = 1.5) +
    labs(title = "Gini mean per group and period (DD estimate)",
         x = "Period", y = "Gini",
         color = "Group") + 
    scale_color_manual(labels = c("No Aguacateros", "Aguacateros"), 
                       values = c("tan3", "seagreen")) +
    scale_x_discrete(labels = c("0" = "Before 2011", "1" = "After 2011")) +
    theme_clean()
)

ggsave("ddplot_estimate_month.png", dd_plot_2, path = "graficas y mapas/Final", dpi = "retina")



