# Logistic model ----------------------------------------------------------
# Packages
library(lattice)
library(ggplot2)
library(caret)

getwd()
setwd("~/OneDrive - Universidad Autónoma Chapingo/1. Doctorado/7.7 Septimo semestre/Seminario VII/I. Art. Modelo logit/Nadie Innova mas/Supplemental material/Logit model")
# look at the directory >

# Data
Rend <- read.csv("Rend1.0.csv")
summary(Rend)
head(Rend)

names(Rend)

# Training set
set.seed(2018)
t.id <- createDataPartition(Rend$Cambio_InAI_Factor2, p=0.7, list = F)

# Modelo logit
mod <- glm(Cambio_InAI_Factor2 ~ Esc18 + Edad18 + Sup18 + CRend + CAuto + CCli + Crad + Cint, data = Rend[t.id, ], family = binomial)
summary(mod)

# Model validation
Rend[-t.id, "PROB_SUCCESS"] <- predict(mod, newdata = Rend[-t.id,], type="response")
Rend[-t.id, "PRED_50"] <- ifelse(Rend[-t.id, "PROB_SUCCESS"]>=0.5, 1, 0)

# Confusion table
table(Rend[-t.id,"Cambio_InAI_Factor2"], Rend[-t.id,"PRED_50"], dnn=c("Actual","Predicho"))

## odds ratios and 95% CI
exp(cbind(OR = coef(mod), confint(mod))) # Forma 1
