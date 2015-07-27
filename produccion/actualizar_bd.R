
fecha <- sqlQuery(con, 'SELECT CONVERT(VARCHAR, GETDATE(), 121)')[1,1] %>%
  as.character %>%
  substr(1,23)
claves <- sort(unique(out$Clav_Hotel))
j <- 1

for(i in claves){
  aux <- out %>%
    filter(Clav_Hotel == i) %>%
    mutate(Fecha_Actualizacion = fecha)
  if(nrow(aux) >= min_num_recom){
    # Actualizar recomendaciones del hotel i
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
    # Borrar recomendaciones antiguas del hotel i
    qry <- sprintf(paste("DELETE FROM RM_PruebaHotelesRecomendaciones",
                         "WHERE Clav_Hotel = %i",
                         "AND Fecha_Actualizacion <> '%s'"), i, fecha)
    sqlQuery(con, qry)
  }
  if(floor(verbose/5) <= length(claves) & j %% floor(verbose/5) == 1){
    cat(paste0('Iter: ', j, '\t\t...\t\t', round(100*j/length(claves)), '%\n'))
  }
  j <- j + 1
}
