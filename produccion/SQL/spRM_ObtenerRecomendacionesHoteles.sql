

-- =============================================

-- Author:     Felipe Gerard

-- Create date: 20150724

-- Description: Obtiene lista de hoteles para alimentar al Sistema de Recomendación de Hoteles. Si el hotel ya tiene recomendaciones
--				del algoritmo nuevo, usa ésas. En otro caso, como respaldo recomienda hoteles en el mismo destino con las mismas
--				estrellas, ordenados por el Sort Order vigente en la fecha proporcionada.

-- Llamada 1:   EXEC spRM_ObtenerRecomendacionesHoteles @clav_hotel = 16, @fecha = '20150801'
-- Llamada 2:	EXEC spRM_ObtenerRecomendacionesHoteles @clav_hotel = 16, @fecha = '2015-08-01'

-- History:

-- Date           Author            Description

-- ---------------------------------------------

-- 20150728		Felipe Gerard		Cambiamos el SP para usar la nueva tabla de recomendaciones (RM_Hoteles_HotelesRecomendaciones), la fecha de actualización y el índice.
-- 20150729		Felipe Gerard		Pequeñas mejoras de desempeño.

-- =============================================

/*CREATE*/ ALTER PROCEDURE [dbo].spRM_ObtenerRecomendacionesHoteles
	@clav_hotel INT
	, @fecha DATE

AS

BEGIN

	-- SET NOCOUNT ON added to prevent extra result sets from

	-- interfering with SELECT statements.

 	SET NOCOUNT ON;

	-- BEGIN CODE -----------------------------------------------------------------------------
	
	-- Hay recomendaciones del algoritmo nuevo?
	DECLARE @nrec INT = (
		SELECT count(*)
		FROM Hoteles_Busquedas_recomendaciones
		WHERE Clav_Hotel = @clav_hotel
	)

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
						SELECT
							so2.clav_hotel
							, so2.clav_destino
							, so2.posicion
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
		FROM RM_Hoteles_HotelesRecomendaciones WITH(NOLOCK, INDEX(IX_RM_Hoteles_HotelesRecomendaciones_Clav_Hotel))
		WHERE Clav_Hotel = @clav_hotel
			AND Fecha_Actualizacion =
			(
				SELECT max(Fecha_Actualizacion)
				FROM RM_Hoteles_HotelesRecomendaciones
				WITH(NOLOCK, INDEX(IX_RM_Hoteles_HotelesRecomendaciones_Clav_Hotel))
				WHERE Clav_Hotel = @clav_hotel
			)
		ORDER BY Prioridad
	END

	-- END CODE -----------------------------------------------------------------------------
	
END

