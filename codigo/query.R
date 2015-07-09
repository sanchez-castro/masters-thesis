
library(RODBC)
library(dplyr)
library(tidyr)

con <- odbcConnect(dsn = 'sysmatrixi',
                   uid = 'bmxddt005062',
                   pwd = '')


# Lista de hoteles --------------------------------------------------------

claves_hoteles <- sqlQuery(con,
"
SELECT distinct hc.Clav_Hotel, h.Nombre_Hotel
FROM Matrix_Reloaded.dbo.Hoteles_Contratos hc with (nolock)
  INNER JOIN Matrix_Reloaded.dbo.Hoteles h with (nolock)
    ON hc.Clav_Hotel = h.Clav_Hotel
  INNER JOIN Matrix_Reloaded.dbo.Hoteles_Servicios hs with (nolock)
    ON hc.Clav_Hotel = hs.Clav_hotel
WHERE
  (
    h.Clav_Pais in ('AR','MX')
    OR (h.Clav_Pais = 'BR' AND hc.tipo_notif in ('A','E','H','K'))
    OR (h.Clav_Pais = 'US' AND hc.tipo_notif in ('A','E','H'))
  )
  AND h.Activo = 1  
  AND h.Internet = 1
  AND h.latitude IS NOT NULL
  AND h.longitude IS NOT NULL
ORDER BY hc.Clav_Hotel
") %>%
  mutate(ID_Hotel = row_number())

lista_hoteles <- dplyr::select(claves_hoteles, Clav_Hotel)

# Categoria alimentos (All-Inclusive) -------------------------------------

aux_hoteles_alimentos <- sqlQuery(con,
"
SELECT
  hct.Clav_Hotel,
  p.Clav_Agrupador,
	pa.Nombre_Agrupador
FROM Matrix_Reloaded.dbo.hoteles_cuartos_Tarifas2 hct
	INNER JOIN Matrix_Reloaded.dbo.Planes p
		ON hct.Clav_Plan = p.Clav_Plan
	INNER JOIN Matrix_Reloaded.dbo.Planes_Agrupadores pa
		ON p.Clav_Agrupador = pa.Clav_Agrupador
GROUP BY Clav_Hotel, p.Clav_Agrupador, Nombre_Agrupador
ORDER BY Clav_Hotel, p.Clav_Agrupador, Nombre_Agrupador
")

planes_agrupadores <- aux_hoteles_alimentos %>%
  dplyr::select(Clav_Agrupador, Nombre_Agrupador) %>%
  unique

# BLDR (Breakfast, Lunch, Dinner, Beverages)
agrupadores_categorias <- matrix(c('AI', 'BLDR', 1, 1, 1, 1,
                                   'BB', 'B---', 1, 0, 0, 0,
                                   'RO', '----', 0, 0, 0, 0,
                                   'MD', 'BLDR', 1, 1, 1, 1,
                                   'SA', 'BLDR', 1, 1, 1, 1,
                                   'CP', 'B---', 1, 0, 0, 0,
                                   'DB', 'B---', 1, 0, 0, 0,
                                   'AG', 'BLDR', 1, 1, 1, 1,
                                   'FP', 'BLD-', 1, 1, 1, 0,
                                   'MP', 'B-D-', 1, 0, 1, 0,
                                   'BD', 'B-D-', 1, 0, 1, 0,
                                   'GA', '----', 0, 0, 0, 0,
                                   'ND', '----', 0, 0, 0, 0),
                                 byrow=T, ncol=6) %>% as.data.frame
names(agrupadores_categorias) <- c('Clav_Agrupador','Categoria_Alimentos','BREAKFAST_INCLUDED','LUNCH_INCLUDED','DINNER_INCLUDED','BEVERAGES_INCLUDED')

hoteles_alimentos <- aux_hoteles_alimentos %>%
  left_join(agrupadores_categorias) %>%
  mutate(Num_Meals_Included = nchar(gsub('-','',Categoria_Alimentos))) %>%
  arrange(Clav_Hotel, desc(Num_Meals_Included)) %>%
  group_by(Clav_Hotel) %>%
  filter(row_number() == 1)

# Precios -----------------------------------------------------------------

# Precios promedio de dos años para calcular la cerca interior
precios_hoteles <- sqlQuery(con,
"
SELECT
  rh.Clav_Hotel,
  sum(rh.Noches) as Volumen_Noches,
  CASE
    WHEN sum(rh.Noches) = 0 THEN NULL
    ELSE sum(rh.Importe_Tot_Dlls)/sum(rh.Noches)
  END as Precio_Dlls,
  CASE
    WHEN sum(rh.Noches) = 0 THEN NULL
    ELSE  sum(rh.Utilidad_Dlls)/sum(rh.Noches)
  END as Utilidad_Dlls,
  '20130618 - 20150617' as Date_Range
FROM Matrix_Reloaded.dbo.Reservacion r with (nolock)
  INNER JOIN Matrix_Reloaded.dbo.res_Hotel rh with (nolock)
    ON r.Clav_Res = rh.Clav_Res
WHERE R.fecha_venta >= '20130618'
  and R.fecha_venta <= '20150617'
  and R.Email not like '%@bestday.com%'
  and R.Email not like '%@hoteldo.com%'
  and R.Email not like '%@e-travelsolution.com%'
  and R.status_reserva <> 'X'
  and R.status_pago='P'
GROUP BY rh.Clav_Hotel
ORDER BY rh.Clav_Hotel
") %>%
  inner_join(claves_hoteles)

# Hoteles -----------------------------------------------------------------

hoteles <- sqlQuery(con,
"
SELECT
  h.Clav_Hotel,
  h.Clav_Estado,
	h.Clav_Pais,
  h.Nombre_Hotel,
	c.Nombre_Ciudad,
	e.Nombre_Estado,
	p.Nombre_Pais,
	h.Codigo_Postal,
	h.Clav_Categoria_Maletas,
	h.Clav_Destino,
	h.Clav_Ubicacion,
	CASE
		WHEN h.latitude is NULL THEN 999
		ELSE h.latitude
	END as latitude,
	CASE
		WHEN h.longitude is NULL THEN 999
		ELSE h.longitude
	END as longitude,
	h.Adult_Only
FROM Matrix_Reloaded.dbo.Hoteles h with (nolock)
	LEFT JOIN Matrix_Reloaded.dbo.Ciudades c with (nolock)
		ON h.Clav_Ciudad = c.Clav_Ciudad
	LEFT JOIN Matrix_Reloaded.dbo.Estados e with (nolock)
		ON h.Clav_Estado = e.Clav_Estado
	LEFT JOIN Matrix_Reloaded.dbo.Paises p with (nolock)
		ON h.Clav_Pais = p.Clav_Pais
ORDER BY h.Clav_Hotel
") %>%
  inner_join(claves_hoteles) %>%
  arrange(Clav_Pais, Clav_Hotel) %>%
  mutate(temp = as.numeric(gsub('[^0-9]', '', Clav_Categoria_Maletas)),
         Estrellas = ifelse(temp >= 10, temp/10, temp)) %>%
  dplyr::select(-temp) %>%
  left_join(precios_hoteles) %>%
  left_join(hoteles_alimentos)

# Hoteles - Servicios -----------------------------------------------------

ruta_mapeo_categorias <- 'datos/mapeo_categorias_servicios.csv'

# Servicios alimenticios (All-Inclusive, etc)
aux <- data.frame(matrix(c('BREAKFAST_INCLUDED', 'Desayuno incluido',
                           'LUNCH_INCLUDED', 'Comida incluida',
                           'DINNER_INCLUDED', 'Cena incluida',
                           'BEVERAGES_INCLUDED', 'Bebidas incluidas'),
                         byrow = T, ncol = 2, dimnames=list(NULL, c('Clav_Servicio','Nombre_Servicio'))))
servicios_alimentos <- hoteles %>%
  dplyr::select(Clav_Hotel, BREAKFAST_INCLUDED, LUNCH_INCLUDED, DINNER_INCLUDED, BEVERAGES_INCLUDED) %>%
  gather(Clav_Servicio, temp, BREAKFAST_INCLUDED, LUNCH_INCLUDED, DINNER_INCLUDED, BEVERAGES_INCLUDED) %>%
  filter(temp == 1) %>%
  dplyr::select(-temp) %>%
  arrange(Clav_Hotel, Clav_Servicio) %>%
  left_join(aux)

# Servicios
aux_servicios <- sqlQuery(con,
"
SELECT
  hs.Clav_Hotel,
  hs.Clav_Servicio,
	s.Nombre_Servicio
FROM Matrix_Reloaded.dbo.Hoteles_Servicios hs WITH (NOLOCK)
	INNER JOIN Matrix_Reloaded.dbo.Servicios s WITH (NOLOCK)
		ON hs.Clav_Servicio = s.Clav_Servicio
WHERE s.esCasa = 1
ORDER BY s.Orden
") %>%
  inner_join(lista_hoteles) %>%
  rbind(servicios_alimentos)

# IDs Servicios y categorias
aux_categorias_servicios <- read.csv(ruta_mapeo_categorias, header = T) %>%
  filter(Categoria != 'ZIGNORE')
claves_categorias <- aux_categorias_servicios %>%
  dplyr::select(Categoria) %>%
  unique %>%
  arrange(Categoria) %>%
  mutate(ID_Categoria = row_number())
claves_servicios <- aux_categorias_servicios %>%
  left_join(claves_categorias)

# Hoteles - Servicios - Categorías
hoteles_servicios <- aux_servicios %>%
  inner_join(claves_hoteles) %>%
  inner_join(claves_servicios) %>%
  arrange(ID_Hotel, ID_Categoria, ID_Servicio)

# Servicios - Hoteles version matriz rala
dimnames <- list(
  claves_servicios %>% .$Clav_Servicio %>% as.character,
  claves_hoteles$Nombre_Hotel
)
servicios_hoteles_sparse <- sparseMatrix(i = hoteles_servicios$ID_Servicio,
                                         j = hoteles_servicios$ID_Hotel,
                                         x = 1,
                                         dimnames = dimnames)

# Categorias de servicios -------------------------------------------------

# IDs de categorias (en sección hoteles - servicios)

# Mapeo Servicios - Categorias (en sección hoteles - servicios)

# Hoteles - Servicios - Categorias
hoteles_servicios_categorias <- hoteles_servicios

# Maximo numero de servicios por categoria
max_num_categ <- claves_servicios %>%
  group_by(ID_Categoria, Categoria) %>%
  summarise(Max_Num = n())

# Hoteles - Categorias
hoteles_categorias <- hoteles_servicios_categorias %>%
  group_by(Clav_Hotel, Nombre_Hotel, ID_Hotel, Categoria, ID_Categoria) %>%
  summarise(Cantidad = n()) %>%
  left_join(max_num_categ) %>%
  group_by(ID_Hotel) %>%
  mutate(Cantidad_Norm = Cantidad / Max_Num,
         Cantidad_Dist = Cantidad / sum(Cantidad)) %>%
  ungroup

# Categorias - Hoteles matrices anchas por cantidad y por cantidad tipo probabilidad
dimnames <- list(
  claves_categorias %>%  .$Categoria %>% as.character,
  claves_hoteles$Nombre_Hotel
)
categorias_hoteles_sparse_cantidad <- sparseMatrix(i = hoteles_categorias$ID_Categoria,
                                                   j = hoteles_categorias$ID_Hotel,
                                                   x = hoteles_categorias$Cantidad,
                                                   dimnames = dimnames)

categorias_hoteles_sparse_prob <- sparseMatrix(i = hoteles_categorias$ID_Categoria,
                                               j = hoteles_categorias$ID_Hotel,
                                               x = hoteles_categorias$Cantidad_Dist,
                                               dimnames = dimnames)

# Salvar los resultados ---------------------------------------------------

odbcCloseAll()
rm(aux, aux_categorias_servicios, aux_hoteles_alimentos, aux_servicios, con, i, j, x, dimnames) # Borramos auxiliares
lh <- function(qqq){
  # Pasarle ls(). Regresa los tamanios en mb
  a <- sapply(qqq, function(x) eval(parse(text = paste0('object.size(',x,')')))) %>%
    data.frame
  names(a) <- 'b'
  a$mb <- format(round(a$b/2^20, 1), )
  a
}
lh(ls())

#setwd("entregable_v1")
#save.image(file = 'datos/datos_completos.Rdata')


hoteles %>%
  group_by(Clav_Pais) %>%
  tally #summarise(count = n())
dim(hoteles)

hoteles_servicios %>%
  group_by(Clav_Servicio) %>%
  tally %>% #summarise(count=n()) %>%
  data.frame



