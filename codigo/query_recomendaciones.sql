
/*
USE [Matrix_Estadisticas]
GO
CREATE NONCLUSTERED INDEX IX_Hoteles_Busquedas_Recomendaciones_Clav_Hotel
ON [dbo].[Hoteles_Busquedas_Recomendaciones] ([Clav_Hotel])
GO
*/

DECLARE @clav_hotel INT = 2789 --90445895 (BROKEN) --10119087 --16--2277
DECLARE @fecha DATE = '2015-08-03'

DECLARE @nrec INT = (
	SELECT count(*)
	FROM Hoteles_Busquedas_recomendaciones
	WHERE Clav_Hotel = @clav_hotel
)

--SELECT @nrec


IF (@nrec = 0) BEGIN							-- NO HAY RECOMENDACIONES DEL ALGORITMO NUEVO
	
	DECLARE @destino INT = 0
	DECLARE @estrellas CHAR(5) = ''
	DECLARE @datos_hotel TABLE (
		Clav_Hotel INT
		, Clav_Destino INT
		, Clav_Categoria_Maletas CHAR(5)
	)

	INSERT INTO @datos_hotel
	SELECT
		Clav_Hotel
		, Clav_Destino
		, Clav_Categoria_Maletas
	FROM Hoteles h WITH(NOLOCK)
	WHERE Clav_Hotel = @clav_hotel

	SET @destino = (SELECT Clav_Destino FROM @datos_hotel)
	SET @estrellas = (SELECT Clav_Categoria_Maletas FROM @datos_hotel)

	SELECT
			Clav_HotelRecomendacion
			, ROW_NUMBER() OVER (ORDER BY Posicion) AS Posicion
		FROM (
			SELECT DISTINCT TOP 50
				h.Clav_Hotel AS Clav_HotelRecomendacion
				, CASE
					WHEN h.Clav_Hotel = @clav_hotel THEN 0
					ELSE ISNULL(so.posicion,1000000)
				  END AS Posicion
			FROM
				Hoteles h
				INNER JOIN hoteles_Destinos hd
					ON h.Clav_Hotel = hd.clav_hotel
				LEFT JOIN
					(
						SELECT *
						FROM BI_SortOrder so2 WITH(NOLOCK)
						WHERE so2.fecha_in <= @fecha
							AND @fecha < so2.fecha_out
							AND so2.clav_destino = @destino
					) so
					ON h.Clav_Hotel = so.Clav_Hotel
						AND h.Clav_Destino = so.Clav_Destino
			WHERE h.Activo = 1
				AND h.Internet = 1
				AND h.Clav_Destino = @destino
				AND hd.clav_destino = @destino 
				AND h.Clav_Categoria_Maletas = @estrellas
		) t
		ORDER BY Posicion
END
ELSE BEGIN										-- SÍ HAY RECOMENDACIONES DEL ALGORITMO NUEVO
	SELECT TOP 50
		Clav_HotelRecomendacion
		, Prioridad AS Posicion
		, 'Nuevo' AS Metodo
	FROM Hoteles_Busquedas_recomendaciones
	WHERE Clav_Hotel = @clav_hotel
END



