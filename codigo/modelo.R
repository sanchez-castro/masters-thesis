
library(pryr)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(ggthemes)
#library(parallel) # No funciona en windows
# library(foreach)
#library(doSNOW)
library(Matrix)
# library(ggmap)

options(scipen = 10) # Solo los numeros grandes en notacion cientifica


# Cargar los datos --------------------------------------------------------

# setwd("entregable_v2")
print(getwd())
load('datos/datos_completos.Rdata')
source('codigo/funciones.R')


# Cálculos previos --------------------------------------------------------

a1 <- hoteles %>%
  dplyr::select(id1=ID_Hotel, n1=Nombre_Hotel, cl1=Clav_Hotel, p1=Precio_Dlls, u1=Utilidad_Dlls,
                range1=Date_Range,
                pais1=Nombre_Pais, est1=Nombre_Estado, zip1=Codigo_Postal, stars1=Estrellas,
                dest1=Clav_Destino, ubic1=Clav_Ubicacion, long1=longitude, lat1=latitude,
                adult1=Adult_Only, allinc1=Categoria_Alimentos)
a2 <- hoteles %>%
  dplyr::select(id2=ID_Hotel, n2=Nombre_Hotel, cl2=Clav_Hotel, p2=Precio_Dlls, u2=Utilidad_Dlls,
                range2=Date_Range,
                pais2=Nombre_Pais, est2=Nombre_Estado, zip2=Codigo_Postal, stars2=Estrellas,
                dest2=Clav_Destino, ubic2=Clav_Ubicacion, long2=longitude, lat2=latitude,
                adult2=Adult_Only, allinc2=Categoria_Alimentos)


# Parámetros globales -----------------------------------------------------

# En paréntesis rango y valor recomendado
alpha <- 0.3811553 # Parámetro de balance (Servicios: 0 -- 1: Perfil). Calculado una única vez
needed_weight <- 30 # (1+, 30) Peso a acumular para generar la cerca interior
price_range <- 0.3  # (0-1, 0.3) Rango de precio de hoteles a tomar en cuenta para cerca interior
num_recom <- 20     # (1+, 20) Número de recomendaciones en precio a dar por hotel
min_num_recom <-    # (1+, 10) Número mínimo de recomendaciones (para hoteles en despoblado)
outer_fence <- 30   # (1+, 30) Radio de la cerca exterior, en kilómetros
nbatch <- 1         # (1+, 1) Número de bloques para partir el cálculo. Disminuye la cantidad de memoria utilizada. Se recomienda para muchos hoteles.


# Modelo completo de recomendaciones --------------------------------------


intervalo <- 1:nrow(hoteles)

recomendados_lista <- lapply(intervalo, function(i){
  if(i %% 50 == 1) print(i)
  line <- hoteles[i,]
  clav <- line$ID_Hotel
  nserv <- filter(hoteles_categorias, ID_Hotel == clav)$Cantidad %>% sum ### MEJORABLE
  
  cand_info <- filtra_cand(clav, hoteles, price_range=price_range,
                            outer_fence = outer_fence, min_num_recom = min_num_recom)
  
  # Candidatos dentro de la cerca exterior o en si se quitó, los primeros min_num_recom
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
                          mat = categorias_hoteles_sparse_cantidad,
                          mat_norm = categorias_hoteles_sparse_prob)
    selected <- cand_outer %>%
      left_join(salida, by=c('id1','id2')) %>%
      mutate(temp = nserv,
             hinge_norm = ifelse(hinge == 0, 0,
                                 ifelse(temp == 0, 1, hinge/nserv)),
             score = alpha*(1 - diverg) + (1 - alpha)*(1 - hinge_norm)) %>%
      dplyr::select(-temp)
    
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
  }
  return(list(selected=selected,         # Hoteles recomendados (ignorando precio)
              id_hotel=clav,             # Clave del hotel original
              nserv=nserv,               # Número de servicios del hotel original
              ncomp=nrow(cand_outer),    # Cantidad de comparaciones efectuadas
              max_km=max_km,             # Radio de la cerca interior
              filtro=cand_info$type))    # Tipo de filtro efectuado
})

recomendados <- lapply(recomendados_lista, function(l) l$selected ) %>%
  rbind_all %>% # Es más rápido primero rbind_all y luego filtrar
  group_by(id1) %>%
  filter(#id1 != id2,
         cumsum(p2 <= (1+price_range)*p1) <= num_recom) %>%
  mutate(rank = row_number()) %>%
  ungroup %>%
  left_join(a1[c('id1','cl1')], by='id1') %>%
  left_join(a2[c('id2','cl2')], by='id2')
recomendados


# Exploración de resultados -----------------------------------------------

if(FALSE){  
  object_size(recomendados)
  object_size(rec)
  length(recomendados)
  recomendados[[sample(1:length(recomendados), 1)]]
  inner <- sapply(recomendados, function(l) nrow(l$selected))
  outer <- sapply(recomendados, function(l) l$ncomp)
  filtros <- sapply(recomendados, function(l) l$filtro)
  
  qplot(inner, binwidth=5)
  qplot(outer)
  qplot(inner, outer)
  round(table(filtros)/length(filtros), 2)
  
  ggplot(rec, aes(score)) +
    geom_histogram()
  
  ids <- unique(rec$id1)
  temp <- filter(rec, id1 %in% sample(ids, 12))
  qplot(score, data=temp) +facet_wrap(~ id1)
  
  filter(hoteles, ID_Hotel == 2424)
  
  # Análisis: Vale la pena poner precios dinámicos? -------------------------
  
  
  x <- recomendados %>%
    group_by(id1) %>%
    filter(p1 != Inf & p2 != Inf) %>%
    mutate(rank = row_number(),
           p_price = p2/p1) %>%
    filter(rank <= 16)
  
  # Hoteles que entran al ranking con descuento del 0%, 10%, 20%...
  descuentos <- 1-c(1,0.9,0.8,0.7,0.6)
  rangos <- (1+price_range)/(1-descuentos)
  aux <- data.frame(rangos, cero=50,
                    lab=paste0(100*descuentos,'%'))
  ggplot(x) +
    geom_bar(aes(p_price)) +
    geom_vline(xintercept=rangos, color='red') +
    geom_text(data=aux,
              aes(rangos-0.05, cero, label=lab),
              angle = 90, color='white') +
    xlim(0,2) +
    facet_wrap(~ rank) +
    labs(title='Histograma de proporción de precio por rango de recomendación.\nLas líneas son los hoteles alcanzados con 0%, 10% y 20% de descuento')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  x1 <- hoteles_categorias %>%
    group_by(ID_Hotel) %>%
    summarise(ncat = n()) %>%
    right_join(hoteles)
  
  ggplot(x1) +
    geom_bar(aes(ncat)) +
    facet_wrap(~Nombre_Pais)
  
  xtabs(~ ncat + Nombre_Pais, data = x1, exclude = NULL, na.action = na.pass)
  xtabs(~ ncat + Nombre_Pais, data = x1, exclude = NULL, na.action = na.pass) %>% apply(2, sum)
  
  
  x2 <- hoteles %>%
    filter(Clav_Pais == 'BR') %>%
    dplyr::select(longitude, latitude)
  
  dim(x2)
  dim(unique(x2))
}
















