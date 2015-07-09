
# Funciones auxiliares ----------------------------------------------------

# Ver el tamaño de los objetos en el workspace, en MB
lh <- function(qqq){
  # Pasarle ls(). Regresa los tamanios en mb
  a <- sapply(qqq, function(x) eval(parse(text = paste0('object.size(',x,')')))) %>%
    data.frame
  names(a) <- 'b'
  a$mb <- format(round(a$b/2^20, 1), )
  a
}

# Imagen de matriz bien rotada
mat_image <- function(mat, ...){
  mat %>%
    apply(2, rev) %>%
    t %>%
    image(...)
}

# Transforma de grados a radianes
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
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

##### Función auxiliar para graficar circulos
generate_circle <- function(center = c(0,0), radius = 1, npoints = 100){
  r = radius
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

##### Calcula el radio para generar un circulo de ciertos km de radio
calculate_radius <- function(long, lat, km, precision = 10000, maxdeg = 2){
  dtheta <- seq(0,maxdeg,l=precision)
  y <- sapply(dtheta, function(i){
    geodesic_distance(long,lat,long,lat+i,units='deg')
  })
  dtheta[y > km][1]
}

##### Función para filtrar hoteles lejanos y caros
filtra_cand <- function(clav,
                        hot,
                        price_range=0.3,
                        outer_fence=50,
                        min_num_recom=10){
  long1 <- hot %>% filter(ID_Hotel == clav) %>% .$longitude
  lat1 <- hot %>% filter(ID_Hotel == clav) %>% .$latitude
  p1 <- hot %>% filter(ID_Hotel == clav) %>% .$Precio_Dlls
  p1 <- ifelse(is.na(p1), Inf, p1) # Si no hay precio, ignoramos el criterio haciendo que cueste infinito
  aux1 <- hot %>%
    mutate(km = geodesic_distance(long1, lat1, longitude, latitude, units = 'deg'),
           ### Sin el ifelse es mucho mas rapido. Requiere informacion limpia
           #ifelse(is.na(latitude) | is.na(longitude), Inf,
                 #      geodesic_distance(long1, lat1, longitude, latitude, units = 'deg')),
           precio = ifelse(is.na(Precio_Dlls), Inf, Precio_Dlls)) %>%
    dplyr::select(ID_Hotel, precio, km)
  precio_idx <- (aux1$precio < (1 + price_range)*p1)
  outer_fence_idx <- (aux1$km <= outer_fence)
  if(sum(precio_idx & outer_fence_idx) >= 10){ # Hay suficientes hoteles cerca en el rango de precios
    out_idx <- (precio_idx & outer_fence_idx)
    flag <- 'dist & price'
  } else if(sum(outer_fence_idx) >= 10){       # No hay suficientes --> Relajamos el precio
    out_idx <- outer_fence_idx
    flag <- 'dist'
  } else {                                   # Aún no hay suficientes --> Tomamos los más cercanos
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
