# File path
getwd()
setwd("~/OneDrive - Universidad Autónoma Chapingo/1. Doctorado/7.7 Septimo semestre/Seminario VII/I. Art. Modelo logit/Nadie Innova mas/Supplemental material/Chord diagram/")
# look at the directory > 

# Library
library(reshape2)
library(circlize)

# Data Generic Innovations
matriz <- read.csv("genericas.csv")
matriz

# Edgelist format with the melt command
longDat1 <-melt(matriz)
longDat1


# Random colours
col_mat = rand_color(length(longDat1), transparency = 0.5)

# Chord diagram
chordDiagram(as.data.frame (longDat1),col = col_mat, annotationTrack = "grid", preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(longDat1))))))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 400) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
  
}, bg.border = NA)


# Data Specialized Innovations
matriz1 <- read.csv("especializadas.csv")
matriz1

# Edgelist format with the melt command
longDat2 <-melt(matriz1)
longDat2

# Random colours
col_mat = rand_color(length(longDat2), transparency = 0.5)

# Chord diagram
chordDiagram(as.data.frame (longDat2),col = col_mat, annotationTrack = "grid", preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(longDat2))))))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 400) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
  
}, bg.border = NA)

# The chord diagram labels were edited in Adobe Illustrator.
