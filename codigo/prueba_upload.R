
library(RODBC)
library(dplyr)
library(lubridate)
con <- odbcConnect(dsn = 'syscubo',
                   uid = 'bmxddt005062',
                   pwd = '')


system.time({
  out_prueba <- out_final %>%
    rename(Prioridad = Rank) %>%
    ungroup %>%
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
})

system.time({
  for(i in sort(unique(out_prueba$Clav_Hotel))){
    # i <- 2
    aux <- out_prueba %>%
      filter(Clav_Hotel == i) %>%
      #mutate(Clav_HotelRecomendacion = -100000000 + Clav_HotelRecomendacion) # para que se vean los cambios
    if(nrow(aux) >= 10){
      qry <- sprintf('DELETE FROM RM_PruebaHotelesRecomendaciones WHERE Clav_Hotel = %i', i)
      sqlQuery(con, qry)
      sqlSave(con,
              dat = aux,
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
  }
})





