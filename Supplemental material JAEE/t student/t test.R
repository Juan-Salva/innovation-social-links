# File path
getwd()
setwd("~/OneDrive - Universidad Autónoma Chapingo/1. Doctorado/7.7 Septimo semestre/Seminario VII/I. Art. Modelo logit/Nadie Innova mas/Supplemental material/t student/")
# look at the directory >

# Data
Data <- read.csv("Rend con ROO t test.csv")
Data

# t-test for variable -----------------------------------------------------
Data$Cambio_InAI_Factor2 <-factor(Data$Cambio_InAI_Factor2,
                                  levels = c(1, 0),
                                  labels = c("Adoptantes", "No adoptantes"))

t.test (Esc18 ~ Cambio_InAI_Factor2, data = Data)
t.test (Edad18 ~ Cambio_InAI_Factor2, data = Data)
t.test (Sup18 ~ Cambio_InAI_Factor2, data = Data)
t.test (CRend ~ Cambio_InAI_Factor2, data = Data)
t.test (CAuto ~ Cambio_InAI_Factor2, data = Data)
t.test (CCli ~ Cambio_InAI_Factor2, data = Data)
t.test (Crad ~ Cambio_InAI_Factor2, data = Data)
t.test (Cint ~ Cambio_InAI_Factor2, data = Data)

#------- SD
#Split / unsplit # Crea una lista con la variable factor
Cambio_InAI_Factor2list <- split(Data, Data$Cambio_InAI_Factor2)
Cambio_InAI_Factor2list["Adoptantes"] # Devuelve la lista
Cambio_InAI_Factor2list["No adoptantes"] # Devuelve la lista

Adoptantes <- Cambio_InAI_Factor2list[["Adoptantes"]] #Devuelve el Data frame, lo puedes guardar en un objeto y exportar a .csv
No_adoptantes <- Cambio_InAI_Factor2list[["No adoptantes"]]

#----- SD de Adoptantes
sd (Adoptantes$Esc18)
sd (Adoptantes$Edad18)
sd (Adoptantes$Sup18)
sd (Adoptantes$CRend)
sd (Adoptantes$CAuto)
sd (Adoptantes$CCli)
sd (Adoptantes$Crad)
sd (Adoptantes$Cint)

#----- SD de No adoptantes
sd (No_adoptantes$Esc18)
sd (No_adoptantes$Edad18)
sd (No_adoptantes$Sup18)
sd (No_adoptantes$CRend)
sd (No_adoptantes$CAuto)
sd (No_adoptantes$CCli)
sd (No_adoptantes$Crad)
sd (No_adoptantes$Cint)
