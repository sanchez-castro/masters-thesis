
library(pryr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Matrix)

options(scipen = 10) # Solo los numeros grandes en notacion cientifica


# Cargar los datos --------------------------------------------------------

# setwd("entregable_v2")
print(getwd())
load('datos/datos_completos.Rdata')
source('codigo/funciones.R')

# Parámetros globales -----------------------------------------------------

# En paréntesis rango y valor recomendado
price_range <- Inf  # (0-1, 0.3) Rango de precio de hoteles a tomar en cuenta para cerca interior
#num_recom <- 20     # (1+, 20) Número de recomendaciones en precio a dar por hotel
min_num_recom <- 10 # (1+, 10) Número mínimo de recomendaciones (para hoteles en despoblado)
outer_fence <- 30   # (1+, 30) Radio de la cerca exterior, en kilómetros
nbatch <- 1         # (1+, 1) Número de bloques para partir el cálculo. Disminuye la cantidad de memoria utilizada. Se recomienda para muchos hoteles.


# Modelo completo de recomendaciones --------------------------------------


intervalo <- 1:nrow(hoteles)

recomendados_alpha_lista <- lapply(intervalo, function(i){
  if(i %% 50 == 1) print(i)
  line <- hoteles[i,]
  clav <- line$ID_Hotel
  nserv <- filter(hoteles_categorias, ID_Hotel == clav)$Cantidad %>% sum ### MEJORABLE
  
  cand_info <- filtra_cand(clav, hoteles, price_range=price_range,
                           outer_fence = outer_fence,
                           min_num_recom = min_num_recom)
  
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
                                 ifelse(temp == 0, 1, hinge/nserv))) %>%
      dplyr::select(-temp) %>%
      arrange(km)
  }
  return(list(selected=selected,         # Hoteles recomendados (ignorando precio)
              id_hotel=clav,             # Clave del hotel original
              nserv=nserv,               # Número de servicios del hotel original
              ncomp=nrow(cand_outer),    # Cantidad de comparaciones efectuadas
              filtro=cand_info$type))    # Tipo de filtro efectuado
})

recomendados_alpha <- lapply(recomendados_lista, function(l) l$selected ) %>%
  rbind_all %>% # Es más rápido primero rbind_all y luego filtrar
  group_by(id1)
  mutate(servicios = 1 - hinge_norm,
         perfil = 1 - diverg) %>%
  filter(servicios != 1)

pca <- recomendados_alpha %>%
  dplyr::select(perfil, servicios) %>%
  as.matrix %>%
  prcomp

rot <- pca$rotation
alpha <- rot[1,1]/sum(rot[,1])
# alpha = 0.3811553

### Gráfica ilustrativa
aux <- recomendados_alpha
new_alpha <- rot[1,1]/sum(rot[,1])
slope <- -1/(-new_alpha/(1-new_alpha))
intercept <- mean(aux$servicios) - slope*mean(aux$perfil)
intercept2 <- mean(aux$servicios) - 1*mean(aux$perfil)
print(new_alpha)
ggplot(aux, aes(perfil, servicios)) +
  geom_point() +
  geom_abline(slope=slope, intercept=intercept, color='red', size=2) +
  geom_abline(slope=1, intercept=intercept2, color='blue', size=2,linetype='dashed') +
  geom_smooth(method='lm', size=2) +
  coord_cartesian(xlim = 0:1, ylim = 0:1)






