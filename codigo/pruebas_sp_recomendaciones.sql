

DECLARE @hot INT = 2789 --2789 (OJO CON FECHA EN EL SORT ORDER)--111492 (NO EN HOTELES)--138071 (NO EN HOTELES)--3--2277--16--10119087 --90445895
DECLARE @fec DATE = '2014-08-05'

-- SP
EXEC spRM_ObtenerRecomendacionesHoteles @clav_hotel = @hot, @fecha = @fec

-- Tabla HOTELES
SELECT
	Clav_Hotel
	, Nombre_Hotel
	, Activo
	, Internet
	, Clav_Destino
FROM Hoteles
WHERE Clav_Hotel = @hot

-- Tabla HOTELES_DESTINOS
SELECT
	clav_hotel
	, clav_destino
FROM hoteles_Destinos
WHERE clav_hotel = @hot



