
library(RODBC)
library(Matrix)
library(dplyr)
library(tidyr)

# Actualizar información --------------------------------------------------


con <- odbcConnect(dsn = 'syscubo',
                   uid = 'bmxddt005062',
                   pwd = 'SQLFelipe142857')


# Hoteles
h1 <- sqlQuery(con, "EXEC spRM_ObtenerDatosHoteles @fecha_ini = '20130724'")
h2 <- h1 %>%
  mutate(ID_Hotel = row_number())

# Hoteles - Categorias
hc1 <- sqlQuery(con, "EXEC spRM_ObtenerHotelesCategoriasServicios")
hc2 <- hc1 %>%
  left_join(h2[c('Clav_Hotel','ID_Hotel')]) %>%
  group_by(Clav_Hotel) %>%
  mutate(Cantidad_Prob = Cantidad / sum(Cantidad)) %>%
  ungroup
claves_categorias <- hc2 %>%
  group_by(Categoria) %>%
  summarise %>%
  mutate(ID_Categoria = row_number())
hc3 <- hc2 %>%
  left_join(claves_categorias)

hoteles <- h2
hoteles_categorias <- hc3


# Categorias - Hoteles matrices anchas por cantidad y por cantidad tipo probabilidad
categorias_hoteles_sparse_cantidad <- sparseMatrix(i = hoteles_categorias$ID_Categoria,
                                                   j = hoteles_categorias$ID_Hotel,
                                                   x = hoteles_categorias$Cantidad)

categorias_hoteles_sparse_prob <- sparseMatrix(i = hoteles_categorias$ID_Categoria,
                                               j = hoteles_categorias$ID_Hotel,
                                               x = hoteles_categorias$Cantidad_Prob)

rm(h1,h2,hc1,hc2,hc3,claves_categorias)
gc()



# Correr modelo -----------------------------------------------------------

source('produccion/funciones.R')

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

rm(r)
gc()



# Preparar resultados para subirlos a la base de datos --------------------

max_num_recom <- 50

out <- recomendados %>%
  mutate(within_price = (p2 < p1 * (1 + 0.3))) %>%
  group_by(cl1) %>%
  mutate(nrec = sum(within_price)) %>%
  filter((nrec >= max_num_recom & within_price) | (nrec < max_num_recom)) %>%
  top_n(n = max_num_recom, rank) %>%
  arrange(cl1, rank) %>%
  dplyr::select(Clav_Hotel=cl1, Clav_HotelRecomendacion=cl2) %>%
  mutate(Prioridad = row_number()) %>%
  ungroup %>%
  mutate(Clav_HotelBusquedaRecomendacion = row_number())


# Actualizar las recomendaciones en la base de datos ----------------------


for(i in sort(unique(out_prueba$Clav_Hotel))){
  aux <- out %>%
    filter(Clav_Hotel == i) %>%
    mutate(Fecha_Actualizacion = as.character(Sys.time()))
    if(nrow(aux) >= 10){
      # Borrar recomendaciones actuales del hotel i
      qry <- sprintf('DELETE FROM RM_PruebaHotelesRecomendaciones WHERE Clav_Hotel = %i', i)
      sqlQuery(con, qry)
      # Actualizar recomendaciones actuales del hotel i
      sqlSave(con,
              dat = aux,
              tablename = 'RM_PruebaHotelesRecomendaciones',
              append = TRUE,
              rownames = FALSE,
              colnames = FALSE,
              verbose = FALSE,
              safer = TRUE,
              fast = TRUE,
              test = FALSE)
    }
}



# Limpiar y cerrar --------------------------------------------------------

odbcCloseAll()
gc()
