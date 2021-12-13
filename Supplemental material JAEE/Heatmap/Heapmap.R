
# Libraries
library (igraph)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(psych)
library(ade4)
library(factoextra)


#Data
Base1 <- read.graph(file.choose("BaseMaiz"), format = "pajek")

bipartite.mapping(Base1)
V(Base1)$type <- bipartite_mapping(Base1)$type

# Convertimos la red de dos modos a la red de un modo
bipartite_matrix <- as_incidence_matrix(Base1)

bipartite_matrix


# Similar to the %in% operator we saw earlier, R gives us a 
# special operator to use for matrix multiplication: %*%.

event_matrix_prod <- t(bipartite_matrix) %*% bipartite_matrix 
diag(event_matrix_prod) <- 0
# The event matrix prod represent the number of persons each
# species "share"

event_matrix_prod
summary(event_matrix_prod)

# Convertimos la red de dos modos a la red de un modo
bipartite_matrix <- as_incidence_matrix(Base1)

bipartite_matrix
bipartitet <- t(bipartite_matrix)
head(bipartitet)
#########################333

distancem <- get_dist(event_matrix_prod)
fviz_dist(distancem, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(event_matrix_prod, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = event_matrix_prod)
##
fviz_nbclust(event_matrix_prod, kmeans, method = "wss")

fviz_nbclust(event_matrix_prod, kmeans, method = "silhouette")

### decidimos que 3 son los ?ptimos

final <- kmeans(event_matrix_prod, 3, nstart = 30)
print(final)

fviz_cluster(final, data = event_matrix_prod)

#Multilicamos la matriz bipartita por su transpuesta
# t(bipartite_matrix)

t(bipartite_matrix)

# Similar to the %in% operator we saw earlier, R gives us a 
# special operator to use for matrix multiplication: %*%.

event_matrix_prod <- t(bipartite_matrix) %*% bipartite_matrix 
diag(event_matrix_prod) <- 0
# The event matrix prod represent the number of persons each
# species "share"

event_matrix_prod
#######################
#K-means clustering

#library(tidyverse)  # data manipulation
#library(cluster)    # clustering algorithms
#library(factoextra) # clustering algorithms & visualization
###########################################################
#library(reshape2)
melted_cormat <- melt(event_matrix_prod)
head(event_matrix_prod)
#library(ggplot2)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(event_matrix_prod){
    Matrizdocumentos[upper.tri(event_matrix_prod)] <- NA
    return(event_matrix_prod)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(event_matrix_prod){
    event_matrix_prod[lower.tri(event_matrix_prod)]<- NA
    return(event_matrix_prod)
}

upper_tri <- get_upper_tri(event_matrix_prod)
upper_tri
# Melt the correlation matrix
#library(reshape2)
melted_cor <- melt(upper_tri, na.rm = TRUE)

######################################################
# Reorder the correlation matrix

#library(reshape2)
#library(tidyverse)

event_matrix_prod <- reorder_cor(event_matrix_prod)
upper_tri <- get_upper_tri(event_matrix_prod)
# Melt the correlation matrix
melted_cor <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cor, aes(Var2, Var1, fill = value))+
    geom_tile(color = "grey")+
    scale_fill_gradient2(low="#FDE725FF", high="#440154FF", mid="#238A8DFF", 
                         midpoint = 2000, limit = c(14,4110), space = "Lab", 
                         name="Number of practices")  +
    theme_minimal()+ # minimal th eme
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed()
# Print the heatmap 
print(ggheatmap)
## adding  numbers and legends
ggheatmap  +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))

########
### EVENT-MATCH SIMILARITY
#######################3
#library(ade4) # If you have not already done so
#library(psych)
bipartite_matrix1 <- as_incidence_matrix(Base1)  # Extract the matrix

event_Q <-YuleCor(bipartite_matrix1)$rho

event_Q <- as.matrix(event_Q) 
event_Q  

#event_correl <- round(cor(event_correl, use="pairwise.complete.obs"), 2)
event_Q <- as.matrix(event_Q)
diag(event_Q) <- 0
event_Q
######dvkdvskdvsk###
#library(reshape2)
melted_cormat <- melt(event_Q)
head(event_Q)
#library(ggplot2)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(event_Q){
    event_Q[upper.tri(event_Q)] <- NA
    return(event_Q)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(event_Q){
    event_Q[lower.tri(event_Q)]<- NA
    return(event_Q)
}

upper_tri <- get_upper_tri(event_Q)
upper_tri
# Melt the correlation matrix
#library(reshape2)
melted_cor <- melt(upper_tri, na.rm = TRUE)

######################################################
# Reorder the correlation matrix

#library(reshape2)
#library(tidyverse)

event_correl <- reorder_cor(event_Q)
upper_tri <- get_upper_tri(event_Q)
# Melt the correlation matrix
melted_cor <- melt(upper_tri, na.rm = TRUE)
melted_cor
# Create a ggheatmap
ggheatmap <- ggplot(melted_cor, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low="red", high="blue", mid="white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Yule's Coefficient of Association")  +
    theme_minimal()+ # minimal th eme
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 10, hjust = 1))+
    coord_fixed()
# Print the heatmap 
print(ggheatmap)
## adding  numbers and legends
ggheatmap +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.7, 0.7),
        legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.7))
#############3
histograma <- read.csv("frecuency.csv",header=TRUE)
histograma

#library(tidyverse)
#library(ggpubr) 
theme_set(
    theme_bw() + 
        theme(legend.position = "top")
)

ggplot(histograma, aes(x = Practice, y = Frecuency)) +
    geom_col() +
    rotate_x_text(angle = 45)

ggplot(histograma, aes(x = reorder(Practice, Frecuency), y = Frecuency)) +
    geom_col()  +
    rotate_x_text(angle = 45)+
    coord_flip()+
    scale_color_gradientn(colours = rainbow(5))

# The heat map labels were edited in Adobe Illustrator.
