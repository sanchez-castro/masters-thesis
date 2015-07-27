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
