library(ggplot2)

out

ant <- sqlQuery(con, "SELECT * FROM [Matrix_Estadisticas].[dbo].[RM_Hoteles_HotelesRecomendaciones]")
ant <- as.tbl(ant)

out
ant


a <- left_join(out, ant, by=c('Clav_Hotel','Clav_HotelRecomendacion')) %>%
  mutate(match = (Prioridad.x==Prioridad.y),
         diff =abs(Prioridad.x - Prioridad.y))

qplot(Prioridad.x, Prioridad.y, data=a)

mean(a$match, na.rm = T)
mean(is.na(a$match))
a %>%
  filter(is.na(a$match)) %>%
  arrange(Clav_Hotel, Prioridad.x)

mean(a$diff, na.rm = T)


c <- read.csv('datos/Hoteles_HotelesRecomendaciones_20151102.csv') %>%
  as.tbl

c
out


x <- left_join(c, out, by=c('Clav_Hotel','Clav_HotelRecomendacion')) %>%
  mutate(match = (Prioridad.x==Prioridad.y),
         diff =abs(Prioridad.x - Prioridad.y))

mean(x$match, na.rm = T)
mean(is.na(x$match))
x %>%
  filter(is.na(x$match)) %>%
  arrange(Clav_Hotel, Prioridad.x)

mean(x$diff, na.rm = T)

