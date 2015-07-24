
/*
USE [Matrix_Estadisticas]
GO
CREATE NONCLUSTERED INDEX IX_Hoteles_Busquedas_Recomendaciones_Clav_Hotel
ON [dbo].[Hoteles_Busquedas_Recomendaciones] ([Clav_Hotel])
GO
*/

DECLARE @hotel INT = 2277 --16--2277
DECLARE @fecha DATE = '2015-08-03'

DECLARE @nrec INT = (
	SELECT count(*)
	FROM Hoteles_Busquedas_recomendaciones
	WHERE Clav_Hotel = @hotel
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
	WHERE Clav_Hotel = @hotel

	SET @destino = (SELECT Clav_Destino FROM @datos_hotel)
	SET @estrellas = (SELECT Clav_Categoria_Maletas FROM @datos_hotel)

	SELECT DISTINCT TOP 20
		h.Clav_Hotel
		, CASE
			WHEN h.Clav_Hotel = @hotel THEN 0
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
				WHERE hh.Clav_Hotel = @hotel
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
				WHEN h.Clav_Hotel = @hotel THEN 0
				ELSE ISNULL(so.posicion,1000000)
			END
		)
		, Clav_Hotel
END
ELSE BEGIN										-- SÍ HAY RECOMENDACIONES DEL ALGORITMO NUEVO
	SELECT TOP 20
		Clav_HotelRecomendacion
		, Prioridad AS Posicion
		, 'Nuevo' AS Algoritmo
	FROM Hoteles_Busquedas_recomendaciones
	WHERE Clav_Hotel = @hotel
END








-------------------









/*
SELECT DISTINCT TOP 20
	h.Clav_Hotel
	, h.Nombre_Hotel
	, h.Clav_Destino
	, h.Clav_Categoria_Maletas
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
			WHERE hh.Clav_Hotel = @hotel
		) opt
		ON opt.Clav_Destino = h.Clav_Destino
			AND opt.Clav_Categoria_Maletas = h.Clav_Categoria_Maletas
			AND opt.Clav_Agrupador = p.Clav_Agrupador
	LEFT JOIN BI_SortOrder so WITH(NOLOCK)
		ON h.Clav_Hotel = so.Clav_Hotel
			AND h.Clav_Destino = so.Clav_Destino
WHERE so.fecha_in <= @fecha
	AND @fecha < so.fecha_out
	AND so.clav_destino = 2
*/



