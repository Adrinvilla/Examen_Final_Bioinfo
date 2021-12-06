
## Todas las secuencias utilizadas.

virus1 <- readBStringSet ("NC_001479.fna") 
length1 <- nchar(virus1)
length1

virus2 <- readBStringSet ("NC_009452.fna") 
length2 <- nchar(virus2)
length2

virus3 <- readBStringSet ("NC_009741.fna") 
length3 <- nchar(virus3)
length3

virus4 <- readBStringSet ("NC_023863.fna") 
length4 <- nchar(virus4)
length4

virus5 <- readBStringSet ("NC_026620.fna") 
length5 <- nchar(virus5)
length5

# Con adist para obtener como en el 2, la distancia entre cada par de secuencias.

v1c2 <- adist(virus1, virus2)
v1c3 <- adist(virus1, virus3)
v1c4 <- adist(virus1, virus4)
v1c5 <- adist(virus1, virus5)
v2c3 <- adist(virus2, virus3)
v2c4 <- adist(virus2, virus4)
v2c5 <- adist(virus2, virus5)
v3c4 <- adist(virus3, virus4)
v3c5 <- adist(virus3, virus5)
v4c5 <- adist(virus4, virus5)

# Hacer la matriz con los daros de las distancias. 5x5

matriz <- matrix(c(0, v1c2, v1c3, v1c4, v1c5, v1c2,
                   0, v2c3, v2c4, dos.cinco, v1c3, v2c3,
                   0, v3c4, v3c5, v1c4, v2c4, v3c4, 0, v4c5,
                   v1c5, v2c5, v3c5, v4c5, 0), nrow = 5, ncol = 5)
matriz

# Con ggplot se hace el mapa de calor

library(ggplot2)
colnames(matriz) <- paste("virus", 1:5)
rownames(matriz) <- paste("virus", 1:5)
heatmap(matriz, scale="column")