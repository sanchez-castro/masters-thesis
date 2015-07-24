

-- =============================================

-- Author:     Felipe Gerard

-- Create date: 20150724

-- Description: Obtiene lista de hoteles para alimentar al Sistema de Recomendación de Hoteles

-- Llamada:     EXEC spRM_ObtenerRecomendacionesHoteles @clav_hotel = 16, @fecha = '20150801'

-- History:

-- Date           Author            Description

-- ---------------------------------------------

-- 

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
		FROM
		(
			SELECT DISTINCT TOP 20
				h.Clav_Hotel AS Clav_HotelRecomendacion
				, CASE
					WHEN h.Clav_Hotel = @clav_hotel THEN 0
					ELSE ISNULL(so.posicion,1000000)
				  END AS Posicion
				--, h.Nombre_Hotel
				--, h.Clav_Destino
				--, h.Clav_Categoria_Maletas
				/* -- OJO QUE SI PONEMOS LA CLAV_AGRUPADOR, ENTONCES SE PUEDEN REPETIR LOS HOTELES
				, p.Clav_Agrupador
				*/
			FROM
				Hoteles h
				INNER JOIN hoteles_cuartos_Tarifas2 hct2 WITH(NOLOCK)
					ON h.Clav_Hotel = hct2.Clav_Hotel
				INNER JOIN Planes p WITH(NOLOCK)
					ON hct2.Clav_Plan = p.Clav_Plan
				INNER JOIN
					(
						SELECT DISTINCT
							hh.Clav_Hotel
							, hh.Clav_Destino
							, hh.Clav_Categoria_Maletas
							, pp.Clav_Agrupador
						FROM Hoteles hh
							INNER JOIN hoteles_cuartos_Tarifas2 hhct2 WITH(NOLOCK)
								ON hh.Clav_Hotel = hhct2.Clav_Hotel
							INNER JOIN Planes pp WITH(NOLOCK)
								ON hhct2.Clav_Plan = pp.Clav_Plan
						WHERE hh.Clav_Hotel = @clav_hotel
					) opt
					ON opt.Clav_Destino = h.Clav_Destino
						AND opt.Clav_Categoria_Maletas = h.Clav_Categoria_Maletas
						AND opt.Clav_Agrupador = p.Clav_Agrupador
				LEFT JOIN BI_SortOrder so WITH(NOLOCK)
					ON h.Clav_Hotel = so.Clav_Hotel
						AND h.Clav_Destino = so.Clav_Destino
			WHERE so.fecha_in <= @fecha
				AND @fecha < so.fecha_out
				AND so.clav_destino = @destino -- Esto no tiene ningún efecto en el resultado pero acelera mucho el query
				AND h.Activo = 1
				--AND h.Clav_Categoria_Maletas = @estrellas -- Esto alenta el query y no tiene ningun efecto en el resultado
			ORDER BY 
				(
					CASE
						WHEN h.Clav_Hotel = @clav_hotel THEN 0
						ELSE ISNULL(so.posicion,1000000)
					END
				)
				, h.Clav_Hotel
		) t
	END
	ELSE BEGIN										-- SÍ HAY RECOMENDACIONES DEL ALGORITMO NUEVO
		SELECT TOP 20
			Clav_HotelRecomendacion
			, Prioridad AS Posicion
		FROM Hoteles_Busquedas_recomendaciones
		WHERE Clav_Hotel = @clav_hotel
	END

	-- END CODE -----------------------------------------------------------------------------
	
END

