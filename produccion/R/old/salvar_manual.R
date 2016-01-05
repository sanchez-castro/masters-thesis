
fecha <- sqlQuery(con, 'SELECT CONVERT(VARCHAR, GETDATE(), 121)')[1,1] %>%
  as.character %>%
  substr(1,23)

aux <- out %>%
  mutate(Fecha_Actualizacion = fecha)

write.table(aux, file='datos/Hoteles_HotelesRecomendaciones_20151102.csv', row.names = F, sep = ',')
