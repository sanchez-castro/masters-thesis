
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
                   pwd = '')

# Actualizar información --------------------------------------------------
source('produccion/R/obtener_info.R')

# Correr modelo -----------------------------------------------------------
source('produccion/R/correr_modelo.R')

# Preparar resultados para subirlos a la base de datos --------------------
source('produccion/R/preparar_salida_version_rapida.R') # [version_completa]

# Actualizar las recomendaciones en la base de datos ----------------------
source('produccion/R/actualizar_bd.R')

# Limpiar y cerrar --------------------------------------------------------

odbcClose(con)
gc()
