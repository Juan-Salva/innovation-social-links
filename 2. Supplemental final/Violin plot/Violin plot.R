# File path
getwd()
setwd("~/OneDrive - Universidad Autónoma Chapingo/1. Doctorado/7.7 Septimo semestre/Seminario VII/I. Art. Modelo logit/Nadie Innova mas/Supplemental material/Violin plot/")
# look at the directory >

# Violin plot -------------------------------------------------------------
# Packages
library(ggplot2)
library(dplyr)

# Data
Data <- read.csv("Rend con ROO.csv")
Data

# Orden por la media de Adoptantes de menor a mayor cambios positivos en su InAI.

Data$Estadoordenado = factor(Data$Estado, levels = c("Quintana Roo", "Guanajuato", "Yucatan", "Hidalgo", "Guerrero",
                                                     "Tlaxcala", "Puebla", "Campeche", "Queretaro", 
                                                     "Michoacan", "Jalisco", "Veracruz", "Chiapas", 
                                                     "Oaxaca", "Mexico"))

ggplot(data=Data, aes(x=Estadoordenado, y = Cambio_InAI, fill=Estadoordenado, cex.axis=2)) +
    #geom_jitter(size = 0.25, color = "gray", alpha = 0.9, fill="black") +
    geom_violin(aes(), color = "black", alpha = 0.4, trim=FALSE) +
    #geom_boxplot(width=0.05, outlier.size = 0, alpha = 0.5, fill = "white", color = "black") +
    scale_y_continuous(breaks=seq(-50, 100, 10)) +
    xlab (NULL) +
    ylab("Changes in InAI (%)") +
    theme(legend.position = "none")
#stat_summary(fun.data=data_summary, size = 0.5)
#stat_summary(fun.y=mean, geom="point", size=2, color="black", alpha = 0.5)
#theme_set(theme_bw())# Fondo

# The violin plot labels were edited in Adobe Illustrator.

