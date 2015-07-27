
library(pryr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Matrix)

options(scipen = 10) # Solo los numeros grandes en notacion cientifica


# Cargar los datos --------------------------------------------------------

print(getwd())
load('temp/datos.Rdata')
source('produccion/funciones.R')

# Modelo completo de recomendaciones --------------------------------------

system.time({
  r <- recomendar(
    hot = hoteles,
    hot_cat = hoteles_categorias,
    mat = categorias_hoteles_sparse_cantidad,
    mat_norm = categorias_hoteles_sparse_prob,
    alpha = 0.3811553,
    needed_weight = 30,
    price_range = 0.3,
    num_recom = 20,
    min_num_recom = 10,
    outer_fence = 30,
    recommend = 'all',
    verbose = 100)
})


info <- r$info
recomendados <- r$recomendados

save(recomendados, info, file = 'temp/salida.Rdata')