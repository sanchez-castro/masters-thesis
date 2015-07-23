
library(pryr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Matrix)

options(scipen = 10) # Solo los numeros grandes en notacion cientifica


# Cargar los datos --------------------------------------------------------

print(getwd())
load('datos/datos_completos.Rdata')
source('codigo/funciones.R')

# Modelo completo de recomendaciones --------------------------------------

rm(list=ls()[!(ls() %in% c('hoteles','hoteles_categorias','categorias_hoteles_sparse_cantidad',
                  'categorias_hoteles_sparse_prob',
                  'recomendar','filtra_cand','calcula_sim','geodesic_distance','deg2rad','lh'))])

system.time({
  r <- recomendar(
    hot = hoteles,
    hot_cat = hoteles_categorias,
    mat = categorias_hoteles_sparse_cantidad,
    mat_norm = categorias_hoteles_sparse_prob,
    alpha = 0.3811553,
    needed_weight = 30,
    price_range = 0.3,
    num_recom = 20,
    min_num_recom = 10,
    outer_fence = 30,
    recommend = 'all',
    verbose = 100)
})


info <- r$info
recomendados <- r$recomendados

dim(info)
info %>% head(20)

dim(recomendados)
recomendados %>% head(10)



# Exploración de resultados -----------------------------------------------

if(FALSE){  
  object_size(recomendados)
  object_size(r$info)
  dim(recomendados)
  # inner = hoteles en la cerca interior (guardamos menos al final)
  # outer = hoteles dentro de la cerca exterior en precio (en caso normal al menos)
  
  qplot(inner, data=r$info, binwidth=5)
  qplot(ncomp, data=r$info)
  qplot(inner, ncomp, data=r$info)
  round(table(r$info$filtro)/sum(table(r$info$filtro)), 2)
  
  ### Por qué hay un salto en ncomp? --> Buenos Aires tiene muchísimos hoteles
  qplot(inner, ncomp, data=r$info)
  x1 <- inner_join(r$info, hoteles)
  x1 %>%
    filter(ncomp > 290) %>%
    group_by(Clav_Pais) %>%
    tally()

  qplot(inner, ncomp, color=Nombre_Estado, size = 7, data=x1) +
    facet_wrap(~ Clav_Pais)
  # Un ejemplo de los argentinos que están todos pegados
  filter(recomendados, id1 == 3595)
  # Veamos qué porcentaje de los hoteles están en zonas pobladas
  hoteles %>%
    group_by(Clav_Pais, Nombre_Ciudad) %>%
    tally %>%
    group_by(Clav_Pais) %>%
    mutate(N = sum(n),
           prop = n/N) %>%
    filter(prop > 0.03) %>%
    View
  
  
  
  ids <- unique(rec$id1)
  temp <- filter(rec, id1 %in% sample(ids, 12))
  qplot(score, data=temp) + facet_wrap(~ id1)
  
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
















