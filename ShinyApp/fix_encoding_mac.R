

rm(list=ls())
load('datos/datos_completos.Rdata')

### ESTO NO SIRVE!!
objs <- ls()

sapply(objs, function(x){
  class(eval(as.name(x)))
})

ix_dfs <- sapply(objs, function(x){
  if('data.frame' %in% class(eval(as.name(x)))){
    return(TRUE)
  } else{
    return(FALSE)
  }
})
dfs <- objs[ix_dfs]

ix_mtxs <- sapply(objs, function(x){
  if('dgCMatrix' %in% class(eval(as.name(x)))){
    return(TRUE)
  } else{
    return(FALSE)
  }
})

for(i in 1:length(dfs)){
  obj <- eval(as.name(dfs[i]))
  if(ix_dfs[i]){
    for(j in 1:ncol(obj)){
      if(is.character(obj[[j]])){
        # Encoding(obj[[j]]) <- 'UTF-8'
        obj[[j]] <- as.character(obj[[j]])
      }
    }
  }
}

### ESTO SÍ SIRVE!!
categorias_hoteles_sparse_cantidad@Dimnames[[2]] <- as.character(categorias_hoteles_sparse_cantidad@Dimnames[[2]])
categorias_hoteles_sparse_prob@Dimnames[[2]] <- as.character(categorias_hoteles_sparse_prob@Dimnames[[2]])
servicios_hoteles_sparse@Dimnames[[2]] <- as.character(servicios_hoteles_sparse@Dimnames[[2]])

save.image('datos/datos_completos_mac.Rdata')




