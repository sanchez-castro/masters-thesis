
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


# Modelo completo de recomendaciones --------------------------------------

rm(list=ls()[!(ls() %in% c('hoteles','hoteles_categorias','categorias_hoteles_sparse_cantidad',
                  'categorias_hoteles_sparse_prob','a1','a2',
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

dim(r$info)
r$info %>% head(20)

recomendados <- r$recomendados %>%
  left_join(a1[c('id1','cl1')], by='id1') %>%
  left_join(a2[c('id2','cl2')], by='id2')

dim(recomendados)
recomendados %>% head(10)


# Versión provisional para subir ya ---------------------------------------

max_num_recom <- 20
out <- r$recomendados %>%
  left_join(a1[c('id1','cl1','n1')], by='id1') %>%
  left_join(a2[c('id2','cl2','n2')], by='id2') %>%
  dplyr::select(cl1,cl2,n1,n2,p1,p2,km,rank,score,diverg,hinge,diff_features) %>%
  mutate(within_price = (p2 < p1 * (1 + 0.3))) %>%
  group_by(cl1) %>%
  mutate(nrec = sum(within_price)) %>%
  filter((nrec >= max_num_recom & within_price) | (nrec < max_num_recom)) %>%
  top_n(n = max_num_recom, rank) %>%
  arrange(cl1, rank)

out_info <- out %>%
  group_by(cl1) %>%
  tally
qplot(n, data=out_info)

out[1001:2000,] %>%
  View

out %>%
  filter(cl1 == 9) %>%
  head(10)

out_final <- out %>%
  dplyr::select(Clav_Hotel_Buscado=cl1, Clav_Hotel_Recomendado=cl2) %>%
  mutate(Rank = row_number())

out_final %>%
  tally %>%
  .$n %>%
  mean
dim(out_final)
out_final$Clav_Hotel_Buscado %>% unique %>% length
write.csv(out_final,
          file = 'salida/recomendaciones_implementacion_rapida.csv',
          row.names = FALSE,
          quote = FALSE,
          fileEncoding = 'utf-8')

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
















