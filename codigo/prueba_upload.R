
library(RODBC)
library(dplyr)
library(lubridate)
con <- odbcConnect(dsn = 'syscubo',
                   uid = 'bmxddt005062',
                   pwd = '')


out_prueba <- out_final[1:1000, ] %>%
  rename(Prioridad = Rank) %>%
  mutate(Clav_HotelBusquedaRecomendacion = row_number()) %>%
  dplyr::select(Clav_HotelBusquedaRecomendacion, Clav_Hotel, Clav_HotelRecomendacion, Prioridad)

sqlSave(con,
        dat = out_prueba,
        tablename = 'RM_PruebaHotelesRecomendaciones',
        #index = 'Clav_Hotel',
        append = TRUE,
        rownames = FALSE, #'Clav_HotelBusquedaRecomendacion',
        colnames = FALSE,
        verbose = FALSE,
        safer = TRUE,
        #addPK = TRUE,
        #typeInfo = rep('integer',4),
        fast = TRUE,
        test = FALSE)


out_prueba2 <- out_prueba %>%
  filter(Clav_Hotel == 2) %>%
  mutate(Clav_HotelRecomendacion = 1000 + row_number())

#sqlQuery(con, 'drop table RM_PruebaHotelesRecomendaciones')


for(i in sort(unique(out_prueba$Clav_Hotel))){
  aux <- out_prueba %>%
    filter(Clav_Hotel == i)
  if(nrow(aux) >= 10){
    sqlQuery(con, 'delete from RM_PruebaHotelesRecomendaciones where Clav_Hotel = 2')
  sqlSave(con,
          dat = out_prueba2,
          tablename = 'RM_PruebaHotelesRecomendaciones',
          #index = 'Clav_Hotel',
          append = TRUE,
          rownames = FALSE, #'Clav_HotelBusquedaRecomendacion',
          colnames = FALSE,
          verbose = FALSE,
          safer = TRUE,
          #addPK = TRUE,
          #typeInfo = rep('integer',4),
          fast = TRUE,
          test = FALSE)
}






