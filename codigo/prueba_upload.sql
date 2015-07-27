--drop table RM_PruebaHotelesRecomendaciones
truncate table RM_PruebaHotelesRecomendaciones

CREATE TABLE RM_PruebaHotelesRecomendaciones (
	Clav_HotelBusquedaRecomendacion INT PRIMARY KEY
	, Clav_Hotel INT
	, Clav_HotelRecomendacion INT
	, Prioridad INT
	, Fecha_Actualizacion DATETIME
	-- Indice por Clav_Hotel
	--, CONSTRAINT IX_PruebaHotelesRecomendaciones_Clav_Hotel PRIMARY KEY CLUSTERED (Clav_Hotel)
)

---------------
USE [Matrix_Estadisticas]
GO
CREATE NONCLUSTERED INDEX [IX_RM_PruebaHotelesRecomendaciones_Clav_Hotel] ON [dbo].[RM_PruebaHotelesRecomendaciones]
(
	[Clav_Hotel] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
GO
----------------

/**/

DECLARE @hotel INT = 2
--DELETE FROM RM_PruebaHotelesRecomendaciones WHERE Clav_Hotel = @hotel

SELECT *
FROM RM_PruebaHotelesRecomendaciones WITH(NOLOCK, INDEX(IX_RM_PruebaHotelesRecomendaciones_Clav_Hotel))
WHERE Clav_Hotel = @hotel


SELECT *
FROM RM_PruebaHotelesRecomendaciones WITH(NOLOCK, INDEX(IX_RM_PruebaHotelesRecomendaciones_Clav_Hotel))
WHERE Clav_HotelRecomendacion < 0

SELECT TOP 100 *
FROM Hoteles_Busquedas_Recomendaciones WITH(NOLOCK)
WHERE Clav_Hotel = @hotel



SELECT count(*)
FROM RM_PruebaHotelesRecomendaciones

SELECT max(Clav_Hotel)
FROM RM_PruebaHotelesRecomendaciones


--drop table RM_PruebaHotelesRecomendaciones

-- delete from tabla where condicion


SELECT TOP 10 *
FROM Hoteles WITH(NOLOCK)
WHERE Clav_Pais = 'AR'


DECLARE @hot INT = 90000809 --2789 (OJO CON FECHA EN EL SORT ORDER)--111492 (NO EN HOTELES)--138071 (NO EN HOTELES)--3--2277--16--10119087 --90445895
DECLARE @fec DATE = '2014-08-05'

-- SP
EXEC spRM_ObtenerRecomendacionesHoteles @clav_hotel = @hot, @fecha = @fec


