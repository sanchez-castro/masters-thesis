
library(tidyr)
library(dplyr)
library(leaflet)



# Hoteles propios ---------------------------------------------------------

load('datos/datos_completos.Rdata')
dim(hoteles)
head(hoteles)

x1 <- hoteles %>%
  group_by(longitude, latitude) %>%
  mutate(num_stacked = n()) %>%
  ungroup %>%
  arrange(desc(num_stacked), longitude, latitude) %>%
  as.data.frame
View(x1)

x2 <- x1 %>%
  filter(num_stacked > 1) %>%
  group_by(longitude, latitude, num_stacked) %>%
  summarise %>%
  mutate(size = 5*sqrt(num_stacked))

## Estadística básica

# Número de hoteles encimados
sum(x2$num_stacked)

# Proporción de hoteles encimados
sum(x2$num_stacked)/nrow(hoteles)

# Todos
x1 %>%
  dplyr::select(longitude, latitude, num_stacked) %>%
  unique %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~(3*sqrt(num_stacked)),
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'blue',
                   fillOpacity = 0.5)

# Encimados
x2 %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~size,
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'blue',
                   fillOpacity = 0.5)



# Hoteles EAN -------------------------------------------------------------

hoteles_ean <- read.csv('datos/Coordenadas_EAN.csv', header=T)
names(hoteles_ean) <- c('Clav_HotelEAN','HotelEAN','latitude','longitude')
hoteles_ean <- hoteles_ean[c(1,2,4,3)]
dim(hoteles_ean)
head(hoteles_ean)

x1_ean <- hoteles_ean %>%
  group_by(longitude, latitude) %>%
  mutate(num_stacked = n()) %>%
  ungroup %>%
  arrange(desc(num_stacked), longitude, latitude) %>%
  as.data.frame
View(x1_ean %>% filter(longitude != 0 | latitude != 0))

x2_ean <- x1_ean %>%
  filter(num_stacked > 1) %>%
  group_by(longitude, latitude, num_stacked) %>%
  summarise %>%
  mutate(size = 5*sqrt(num_stacked))


## Estadística básica

# Número de hoteles encimados 
sum(x2_ean$num_stacked)
# Proporción de hoteles encimados
sum(x2_ean$num_stacked)/nrow(hoteles_ean)

# Número de hoteles encimados (sin contar los que están en (0, 0))
sum(x2_ean %>% filter(longitude != 0 | latitude != 0) %>% .$num_stacked)
# Proporción de hoteles encimados (sin contar los que están en (0, 0))
sum(x2_ean %>% filter(longitude != 0 | latitude != 0) %>% .$num_stacked)/nrow(hoteles_ean %>% filter(longitude != 0 | latitude != 0))

# Todos
x1_ean %>%
  filter(longitude != 0 | latitude != 0) %>%
  dplyr::select(longitude, latitude, num_stacked) %>%
  unique %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~(3*sqrt(num_stacked)),
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'blue',
                   fillOpacity = 0.5)


# Encimados
x2_ean %>%
  filter(longitude != 0 | latitude != 0) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~size,
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'red',
                   fillOpacity = 0.5)




# Juntando ----------------------------------------------------------------

# Cuántos pegan por coordenadas? --> Muy pocos

z1 <- hoteles %>%
  filter(longitude != 0 | latitude != 0) %>%
  inner_join(hoteles_ean) %>%
  dplyr::select(Clav_Hotel, Nombre_Hotel, Nombre_Ciudad, Nombre_Estado, Nombre_Pais, longitude, latitude, Clav_HotelEAN, HotelEAN)
View(z1)

# Todos

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~(3*sqrt(num_stacked)),
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'red',
                   fillOpacity = 0.5,
                   data=x1_ean %>%
                     filter(longitude != 0 | latitude != 0) %>%
                     dplyr::select(longitude, latitude, num_stacked) %>%
                     unique) %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~(3*sqrt(num_stacked)),
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'blue',
                   fillOpacity = 0.5,
                   data=x1 %>%
                     dplyr::select(longitude, latitude, num_stacked) %>%
                     unique)

# Encimados
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~size,
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'blue',
                   fillOpacity = 0.5,
                   data=x2) %>%
  addCircleMarkers(lng = ~longitude,
                   lat = ~latitude,
                   radius = ~size,
                   stroke = F,
                   popup = ~as.character(num_stacked),
                   fillColor = 'red',
                   fillOpacity = 0.5,
                   data=x2_ean %>% filter(longitude != 0 | latitude != 0))








