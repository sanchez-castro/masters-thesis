
fecha <- sqlQuery(con, 'SELECT CONVERT(VARCHAR, GETDATE(), 121)')[1,1] %>%
  as.character %>%
  substr(1,23)
claves <- sort(unique(out$Clav_Hotel))
idx_claves <- round(seq(1, length(claves), length.out = length(claves)/(500/max_num_recom)))

j <- 0
reg <- 0

for(i in 1:(length(idx_claves) - 1)){
  cl <- claves[(idx_claves[i] + as.numeric(i!=1)):idx_claves[i+1]]
  aux <- out %>%
    filter(Clav_Hotel %in% cl) %>%
    mutate(Fecha_Actualizacion = fecha)
  if(nrow(aux) >= min_num_recom){
    # Actualizar recomendaciones del hotel i
    sqlSave(con,
            dat = aux,
            tablename = 'RM_Hoteles_HotelesRecomendaciones',
            append = TRUE,
            rownames = FALSE,
            colnames = FALSE,
            verbose = FALSE,
            safer = TRUE,
            fast = TRUE,
            test = FALSE)
  }
  
  j <- j + length(cl)
  reg <- reg + nrow(aux)
  cat(paste0('# hoteles: ', j, '\t\t...\t\t# renglones: ', reg, '\t\t...\t\t', round(100*reg/nrow(out),1), '%\n'))
}

# Borrar recomendaciones antiguas
##### FALTA CHECAR SI SE INSERTARON TODAS

qry_count_new <- sprintf(paste("SELECT count(*)",
                               "FROM RM_Hoteles_HotelesRecomendaciones",
                               "WHERE Fecha_Actualizacion = '%s'"), fecha)
num_new <- sqlQuery(con, qry_count_new)[1,1]

if(nrow(out) == num_new){
  # Calcular cuántos deletes de 5000 se necesitan
  qry_count_old <- sprintf(paste("SELECT count(*)",
                                 "FROM RM_Hoteles_HotelesRecomendaciones",
                                 "WHERE Fecha_Actualizacion <> '%s'"), fecha)
  num_old <- sqlQuery(con, qry_count_old)[1,1]
  
  for(k in 1:ceiling(num_old/5000)){
    qry_delete <- sprintf(paste("DELETE TOP (5000)",
                                "FROM RM_Hoteles_HotelesRecomendaciones",
                                "WHERE Fecha_Actualizacion <> '%s'"), fecha)
    sqlQuery(con, qry_delete)
  }
} else {
  cat('No se borraron porque no coincide la salida con lo insertado...')
}


