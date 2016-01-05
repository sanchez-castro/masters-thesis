
obtener_informacion <- function(country='MX'){
  # Hoteles
  sp.call.getdata <- paste0("EXEC spRM_ObtenerDatosHoteles @fecha_ini = '20130724', @country = '", country, "'")
  h1 <- sqlQuery(con, sp.call.getdata)
  h2 <- h1 %>%
    mutate(ID_Hotel = row_number())
  
  # Hoteles - Categorias
  sp.call.getserv <- paste0("EXEC spRM_ObtenerHotelesCategoriasServicios @country = '", country, "'")
  hc1 <- sqlQuery(con, sp.call.getserv)
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
  
  return(list(claves_categorias=claves_categorias,
              hoteles=hoteles,
              hoteles_categorias=hoteles_categorias,
              categorias_hoteles_sparse_cantidad=categorias_hoteles_sparse_cantidad,
              categorias_hoteles_sparse_prob=categorias_hoteles_sparse_prob))
}
# rm(h1,h2,hc1,hc2,hc3,claves_categorias)
# gc()
