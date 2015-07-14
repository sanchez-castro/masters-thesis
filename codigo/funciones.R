
# Funciones auxiliares
##################################################################



##### Ver el tamaño de los objetos en el workspace, en MB
## In: vector de caracteres con nombres de objetos en el workspace (por ejemplo, ls())
## Out: un data.frame con información del tamaño de los objetos
lh <- function(qqq){
  # Pasarle ls(). Regresa los tamanios en mb
  a <- sapply(qqq, function(x) eval(parse(text = paste0('object.size(',x,')')))) %>%
    data.frame
  names(a) <- 'b'
  a$mb <- format(round(a$b/2^20, 1), )
  a
}



##### Transforma de grados a radianes
deg2rad <- function(deg) return(deg*pi/180)



##### Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
geodesic_distance <- function(long1, lat1, long2, lat2, units = 'deg') {
  if(any(is.na(c(long1,lat1,long2,lat2)))){
    return(Inf)
  } else {
    if(units == 'deg'){
      long1 <- deg2rad(long1)
      lat1 <- deg2rad(lat1)
      long2 <- deg2rad(long2)
      lat2 <- deg2rad(lat2)
    }
    R <- 6371 # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    #   c <- 2 * asin(min(1,sqrt(a)))
    # el ifelse es 7 veces más rápido que sapply...
    c <- 2 * asin(ifelse(a > 1, 1, sqrt(a))) #asin(sapply(a, function(x) min(1,sqrt(x))))
    d = R * c
    return(d) # Distance in km
  }
}



##### Función para filtrar hoteles lejanos y caros
## In.
# clav: ID_Hotel del hotel original
# hot: tabla de hoteles
# price_range: % del precio hacia arriba que se permite
# outer_fence: radio de la cerca exterior en km
# min_num_recom: mínimo número de recomendaciones a guardar (para zonas despobladas)
## Out.
# Una lista con los siguientes componentes:
#    cand: lista completa de parejas (original, hotel_i)
#    precio_idx: índices de cand que cumplen con el criterio de precio
#    outer_fence_idx: índices de cand dentro de la cerca exterior
#    out_idx: índices de cand con los hoteles candidatos iniciales
#    price_range: price_range de entrada
#    outer_fence: outer_fence de entrada
#    min_num_recom: min_num_recom de entrada
#    type:
filtra_cand <- function(clav,
                        hot,
                        price_range=0.3,
                        outer_fence=50,
                        min_num_recom=10){
  # Datos del hotel original
  long1 <- hot %>% filter(ID_Hotel == clav) %>% .$longitude
  lat1 <- hot %>% filter(ID_Hotel == clav) %>% .$latitude
  p1 <- hot %>% filter(ID_Hotel == clav) %>% .$Precio_Dlls
  p1 <- ifelse(is.na(p1) | p1 <= 0, Inf, p1) # Si no hay precio, ignoramos el criterio haciendo que cueste infinito
  
  # Calcular distancia del hotel a los demás
  aux1 <- hot %>%
    mutate(km = geodesic_distance(long1, lat1, longitude, latitude, units = 'deg'),
           ### Sin el ifelse es mucho mas rapido. Requiere informacion limpia
           #ifelse(is.na(latitude) | is.na(longitude), Inf,
                 #      geodesic_distance(long1, lat1, longitude, latitude, units = 'deg')),
           precio = ifelse(is.na(Precio_Dlls), Inf, Precio_Dlls)) %>%
    dplyr::select(ID_Hotel, precio, km)
  
  # Calcular índices de diversos filtros
  precio_idx <- (aux1$precio < (1 + price_range)*p1)
  outer_fence_idx <- (aux1$km <= outer_fence)
  
  # Diversos casos del filtro (paracaídas en caso de que haya pocos hoteles)
  if(sum(precio_idx & outer_fence_idx) >= 10){ # Hay suficientes hoteles cerca en el rango de precios
    out_idx <- (precio_idx & outer_fence_idx)
    flag <- 'dist & price'
  } else if(sum(outer_fence_idx) >= 10){       # No hay suficientes --> Relajamos el precio
    out_idx <- outer_fence_idx
    flag <- 'dist'
  } else {                                     # Aún no hay suficientes --> Tomamos los más cercanos
    out_idx_aux <- order(aux1$km) %>% head(min_num_recom)
    out_idx <- rep(FALSE, nrow(hot))
    out_idx[out_idx_aux] <- TRUE
    flag <- paste('n closest')
  }
  cand <- data.frame(id1=clav, id2=hot$ID_Hotel, km=aux1$km, p1=p1, p2=aux1$precio)
  output <- list(cand=cand,
                 precio_idx=precio_idx,
                 outer_fence_idx=outer_fence_idx,
                 out_idx=out_idx,
                 price_range=price_range,
                 outer_fence=outer_fence,
                 min_num_recom=min_num_recom,
                 type=flag)
  return(output)
}



##### Función para calcular similitudes
## In.
# candidatos: matriz de nx2, con (ID_Hotel_1, ID_Hotel_2) en cada renglón
# mat: matriz (rala o no) de categorías por hoteles
# mat_norm: mat pero cada columna normalizada por el máximo
## Out.
# output: data.frame con candidatos, más divergencia, norma 1 y hinge entre los candidatos.
calcula_sim <- function(candidatos, mat, mat_norm){
  # candidatos es una matriz de nx2, con #candidato 1, #candidato 2
  # mat es la matriz categorias-hoteles
  # mat_norm es mat pero normalizada a que cada hotel sume 1
  
  candidatos <- as.matrix(candidatos)
  if(nrow(candidatos) == 0){
    return(NULL)
  }
  
  # Resta matriz cantidades
  subst <- mat[,candidatos[,1]] - mat[,candidatos[,2]]
  if(length(subst@x) != 0){
    auxm <- subst
    auxm@x <- ifelse(subst@x > 0, subst@x, 0) ###sapply(subst@x, function(x) max(x,0))
    hinge <- colSums(auxm) %>% as.numeric
    auxm@x <- abs(subst@x) ###sapply(subst@x, function(x) abs(x))
    abs <- colSums(auxm) %>% as.numeric
    
    # Resta matriz normalizada
    subst <- mat_norm[,candidatos[,1]] - mat_norm[,candidatos[,2]]
    auxm@x <- abs(subst@x)/2 ###sapply(subst@x, function(x) abs(x)/2)
    diverg <- colSums(auxm) %>% as.numeric
  } else { # Tanto el query como los candidatos tienen cero servicios
    hinge <- rep(0, ncol(subst))
    abs <- hinge
    diverg <- hinge
  }
  
  distancias <- data.frame(diverg=diverg, diff_features=abs, hinge=hinge)

  output <- cbind(candidatos[,1:2], distancias)
  names(output) <- c('id1','id2','diverg','diff_features','hinge')
  
  output
}



##### Función para generar recomendaciones
## In.
# hot: tabla de hoteles
# hot_cat: tabla de hoteles_categorias
# mat: matriz de categorias_hoteles con cantidades (categorias_hoteles_sparse_cantidad)
# mat_norm: matriz de categorias_hoteles con cantidades normalizadas por columna (categorias_hoteles_sparse_prob)
# alpha: ponderacion a poner sobre similitud en perfil vs en servicios
# needed_weight: score a acumular dentro de la cerca interior
# price_range: % del precio hacia arriba que se permite 
# num_recom: % número de recomendaciones que se quiere hacer al final (se pueden regresar más o menos, dependiendo de la densidad y el precio del hotel relativo a la zona. Ver documentación)
# min_num_recom: mínimo número de recomendaciones a guardar (para zonas despobladas)
# outer_fence: radio de la cerca exterior en km
# recommend: 'all' para todos los hoteles (recomendado) o un vector 1:k para los primeros k que aparecen en hot En general, un vector con los hoteles a recomendar, según renglones de tabla hot.
# verbose: número de iteraciones entre mensaje y mensaje de avance
recomendar <- function(
  hot                  # hoteles
  , hot_cat            # hoteles_categorias
  , mat                # categorias_hoteles_sparse_cantidad
  , mat_norm           # categorias_hoteles_sparse_prob
  , alpha = 0.3811553
  , needed_weight = 30
  , price_range = 0.3
  , num_recom = 20
  , min_num_recom = 10
  , outer_fence = 30
  , recommend = 'all'
  , verbose = 100){
  
  if(recommend[1] == 'all') intervalo <- 1:nrow(hot)
  else intervalo <- recommend
  if(!is.numeric(verbose) | verbose < 0 | verbose > length(intervalo)) verbose <- length(intervalo) + 1
  
  recomendados_lista <- lapply(intervalo, function(i){
    if(verbose <= length(intervalo) & i %% verbose == 1){
      cat(paste0('Iter: ', i, '\t\t...\t\t', round(100*i/length(intervalo)), '%\n'))
    }
    line <- hot[i,]
    clav <- line$ID_Hotel
    nserv <- filter(hot_cat, ID_Hotel == clav)$Cantidad %>% sum ### MEJORABLE
    
    cand_info <- filtra_cand(clav, hot, price_range=price_range,
                             outer_fence = outer_fence, min_num_recom = min_num_recom)
    
    # Candidatos dentro de la cerca exterior o si se quitó, los primeros min_num_recom más cercanos
    if(cand_info$type == 'n closest'){
      outer_idx <- cand_info$out_idx
      cand_idx <- rep(TRUE, sum(outer_idx))
    } else if(cand_info$type == 'dist'){
      outer_idx <- cand_info$outer_fence_idx
      cand_idx <- rep(TRUE, sum(outer_idx))
    } else if(cand_info$type == 'dist & price') {
      outer_idx <- cand_info$outer_fence_idx
      cand_idx <- cand_info$precio_idx[cand_info$outer_fence_idx]
    }
    cand_outer <- cand_info$cand[outer_idx,]
    
    if(nrow(cand_outer) <= 1){
      selected <- data.frame()
    } else {
      
      salida <- calcula_sim(cand_outer,
                            mat = mat,
                            mat_norm = mat_norm)
      selected <- cand_outer %>%
        left_join(salida, by=c('id1','id2')) %>%
        mutate(temp = nserv,
               hinge_norm = ifelse(hinge == 0, 0,
                                   ifelse(temp == 0, 1, hinge/nserv)),
               score = alpha*(1 - diverg) + (1 - alpha)*(1 - hinge_norm)) %>%
        dplyr::select(-temp)
      
      ncomp <- nrow(selected)
      if(cand_info$type == 'n closest'){
        selected <- selected %>%
          arrange(km)
      } else if(cand_info$type == 'dist'){
        selected <- selected %>%
          arrange(desc(score))
      } else if(cand_info$type == 'dist & price') {
        selected <- selected %>%
          arrange(km) %>%
          filter(cumsum(score*cand_idx) < needed_weight) %>%
          arrange(desc(score))
      }
      
      max_km <- max(selected$km)
      inner <- nrow(selected)
    }
    return(list(selected=selected,         # Hoteles recomendados (ignorando precio)
                ID_Hotel=clav,             # Clave del hotel original
                nserv=nserv,               # Número de servicios del hotel original
                ncomp=ncomp,               # Cantidad de comparaciones efectuadas
                inner=inner,               # Cantidad de hoteles en la cerca interior
                max_km=max_km,             # Radio de la cerca interior
                filtro=cand_info$type))    # Tipo de filtro efectuado
  })
  
  recomendados <- lapply(recomendados_lista, function(l) l$selected ) %>%
    rbind_all %>% # Es más rápido primero rbind_all y luego filtrar
    group_by(id1) %>%
    filter(#id1 != id2,
      cumsum(p2 <= (1+price_range)*p1) <= num_recom) %>%
    mutate(rank = row_number()) %>%
    ungroup
  info <- data.frame(
    ID_Hotel = sapply(recomendados_lista, function(l) l$ID_Hotel)
    , nserv = sapply(recomendados_lista, function(l) l$nserv)
    , ncomp = sapply(recomendados_lista, function(l) l$ncomp)
    , inner = sapply(recomendados_lista, function(l) l$inner)
    , max_km = sapply(recomendados_lista, function(l) l$max_km)
    , filtro = sapply(recomendados_lista, function(l) l$filtro))
  
  list(recomendados=recomendados, info=info)
}











