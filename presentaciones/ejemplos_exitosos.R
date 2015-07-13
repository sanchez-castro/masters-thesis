
dim(recomendados)
head(hoteles)
filter(hoteles, grepl('Presidente InterContinental Ci', Nombre_Hotel))
filter(hoteles, grepl('Cozumel Palace', Nombre_Hotel))$ID_Hotel
filter(hoteles, grepl('Iberostar Canc', Nombre_Hotel))$ID_Hotel
clav <- 48    # ID_Hotel


filter(recomendados, id1 == clav) %>%
  left_join(a1[c('cl1','n1','stars1','long1','lat1')], by='cl1') %>%
  left_join(a2[c('cl2','n2','stars2','long2','lat2')], by='cl2') %>%
  mutate(km = round(km, 1),
         p2 = round(p2)) %>%
  dplyr::select('Recomendación' = n2,
                'Distancia (km)' = km,
                'Precio (USD)' = p2,
                Estrellas=stars2) %>%
  head(10) %>%
  View