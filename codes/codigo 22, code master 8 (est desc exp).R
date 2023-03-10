#### Tesis #####
##### Codigo 23: code master 9 (est desc exportaciones)#####

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


### cargamos base de datos
exp <- read_xlsx("data/Datos Agricolas/Exp_Ag_clean.xlsx")

### exploramos
summary(exp)

### graficamos
(exp_plot <- ggplot(data = exp,
                   aes(as_date(fecha), miles_dolares)) +
    geom_line(color = "seagreen") +
    labs(title = "Value of Avocado Exports",
         subtitle = "Mexico",
         x = "Date", y = "Thousands of dollars",
         caption = "Source: BANXICO")
    + scale_x_date(labels = date_format("%Y-%m-%d"))
  + geom_vline(xintercept= ymd(20110101), linetype=2, show.legend = TRUE)
  + geom_text(aes(x = ymd(20120502), label="2011", y=300000), angle=0, color = "red")
  + theme_clean()
)

ggsave("exp_avo_mex.png", exp_plot, path = "graficas y mapas/Final/Context", dpi = "retina")
