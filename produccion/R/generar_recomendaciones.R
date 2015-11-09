
library(RODBC)
library(Matrix)
library(dplyr)
library(tidyr)

source('produccion/R/funciones.R')

# Parámetros --------------------------------------------------------------

max_num_recom <- 20
min_num_recom <- 10
alpha <- 0.3811553
needed_weight <- 30
price_range <- 0.3
outer_fence <- 30
recommend <- 'all'
verbose <- 100
tempdir <- 'temp'
paises <- c('MX','AR','US','BR')
archivo_salida <- 'datos/Hoteles_HotelesRecomendaciones_20151109.csv'

con <- odbcConnect(dsn = 'syscubo',
                   uid = 'bmxddt005062',
                   pwd = '')



# Correr el resultado por país --------------------------------------------

salida <- list()
for(pais in paises){
  cat(paste0('---------------------------------------\n'))
  cat(paste0('Procesando ', pais, '...\n'))
  
  # Obtener información de la base de datos
  print(system.time({
    dat <- obtener_informacion(pais)
  }))
  
  # Generar recomendaciones
  cat(paste0('Generando recomendaciones de ', pais, '...\n'))
  print(system.time({
    r <- recomendar(
      hot = dat$hoteles,
      hot_cat = dat$hoteles_categorias,
      mat = dat$categorias_hoteles_sparse_cantidad,
      mat_norm = dat$categorias_hoteles_sparse_prob,
      alpha = alpha,
      needed_weight = needed_weight,
      price_range = price_range,
      num_recom = max_num_recom,
      min_num_recom = min_num_recom,
      outer_fence = outer_fence,
      recommend = recommend,
      verbose = verbose)
    info <- r$info
    recomendados <- r$recomendados
  }))
  
  # Prerarar la salida
  cat(paste0('Preparando la salida de ', pais, '...\n'))
  print(system.time({
    salida[[pais]] <- recomendados %>%
      mutate(within_price = (p2 < p1 * (1 + 0.3))) %>%
      group_by(cl1) %>%
      mutate(nrec = sum(within_price)) %>%
      filter(((nrec >= max_num_recom) & within_price) | (nrec < max_num_recom)) %>%
      top_n(n = max_num_recom, desc(rank)) %>%
      arrange(cl1, rank) %>%
      dplyr::select(Clav_Hotel=cl1, Clav_HotelRecomendacion=cl2) %>%
      mutate(Prioridad = row_number()) %>%
      ungroup
  }))
  
  # Guardar la salida por si el proceso falla a la mitad
  cat(paste0('Guardando la salida de ', pais, '...\n'))
  aux <- list(dat=dat, r=r, salida=salida[[pais]])
  save(aux, file=paste0(tempdir,'/temp_',pais,'.RData'))
}



# Juntar los resultados de todos los países -------------------------------
out <- rbind_all(salida)


# Guardar resultados al filesystem local ----------------------------------
fecha <- sqlQuery(con, 'SELECT CONVERT(VARCHAR, GETDATE(), 121)')[1,1] %>%
  as.character %>%
  substr(1,23)

aux <- out %>%
  mutate(Fecha_Actualizacion = fecha)

write.table(aux, file=archivo_salida, row.names = F, sep = ',')


# Limpiar y cerrar --------------------------------------------------------

odbcClose(con)
gc()
