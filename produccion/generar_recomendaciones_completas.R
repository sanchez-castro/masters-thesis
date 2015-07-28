
library(RODBC)
library(Matrix)
library(dplyr)
library(tidyr)


# Parámetros --------------------------------------------------------------

max_num_recom <- 20
min_num_recom <- 10
alpha <- 0.3811553
needed_weight <- 30
price_range <- 0.3
outer_fence <- 30
recommend <- 'all'
verbose <- 100

con <- odbcConnect(dsn = 'syscubo',
                   uid = 'bmxddt005062',
                   pwd = 'SQLFelipe142857')

# Actualizar información --------------------------------------------------
source('produccion/obtener_info.R')

# Correr modelo -----------------------------------------------------------
source('produccion/correr_modelo.R')

# Preparar resultados para subirlos a la base de datos --------------------
source('produccion/preparar_salida_version_completa.R')

# Actualizar las recomendaciones en la base de datos ----------------------
source('produccion/actualizar_bd.R')

# Limpiar y cerrar --------------------------------------------------------

odbcClose(con)
gc()
