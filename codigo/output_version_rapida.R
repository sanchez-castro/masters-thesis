
# Correr modelo.R y después...

# Versión provisional para subir ya ---------------------------------------

max_num_recom <- 20
out <- r$recomendados %>%
  mutate(within_price = (p2 < p1 * (1 + 0.3))) %>%
  group_by(cl1) %>%
  mutate(nrec = sum(within_price)) %>%
  filter((nrec >= max_num_recom & within_price) | (nrec < max_num_recom)) %>%
  top_n(n = max_num_recom, rank) %>%
  arrange(cl1, rank)

out_final <- out %>%
  dplyr::select(Clav_Hotel=cl1, Clav_HotelRecomendado=cl2) %>%
  mutate(Rank = row_number())

out_final$Clav_Hotel %>% unique %>% length
# write.csv(out_final,
#           file = 'salida/recomendaciones_implementacion_rapida.csv',
#           row.names = FALSE,
#           quote = FALSE,
#           fileEncoding = 'utf-8')

# Para corroborar recomendaciones: pegamos nombres ------------------------

library(RODBC)
con <- odbcConnect(dsn = 'syscubo',
                   uid = 'bmxddt005062',
                   pwd = '')
a1 <- sqlQuery(con, "
SELECT Clav_Hotel cl1, Nombre_Hotel n1
FROM Matrix_Estadisticas.dbo.Hoteles with (nolock)
")
odbcClose(con); rm(con)
a2 <- a1 %>% rename(cl2=cl1, n2=n1)

out_names <- out %>%
  left_join(a1) %>%
  left_join(a2) %>%
  .[c(1:4,16:17,5:15)]

out_info <- out_names %>%
  group_by(cl1) %>%
  tally
qplot(n, data=out_info)

out_names[1001:2000,] %>%
  View

out_names %>%
  filter(cl1 == 9) %>%
  head(10)

out_final %>%
  tally %>%
  .$n %>%
  mean
dim(out_final)

