

-- =============================================

-- Author:     Felipe Gerard

-- Create date: 20150723

-- Description: Obtiene lista de hoteles para alimentar al Sistema de Recomendación de Hoteles

-- Llamada:     EXEC spRM_ObtenerHotelesCategoriasServicios

-- History:

-- Date           Author            Description

-- ---------------------------------------------

-- 

-- =============================================

/*CREATE*/ ALTER PROCEDURE [dbo].spRM_ObtenerHotelesCategoriasServicios 
@country as CHAR(2)

AS

BEGIN

	-- SET NOCOUNT ON added to prevent extra result sets from

	-- interfering with SELECT statements.

 	SET NOCOUNT ON;

	-- BEGIN CODE -----------------------------------------------------------------------------
		
	DECLARE @hot_serv TABLE (
		Clav_Hotel INT
		, Clav_Servicio VARCHAR(20)
	)

	DECLARE @listaHoteles AS TABLE (
		Clav_Hotel INT
	)

	-- Lista de hoteles que queremos (por geografía, etc)
	INSERT INTO @listaHoteles
	EXEC spRM_ObtenerListaDeHoteles @country

	-- Generamos los servicios de plan de alimentos y los insertamos en @hot_serv
	INSERT INTO @hot_serv
	SELECT
		Clav_Hotel
		, Clav_Servicio
	FROM (
		SELECT
			hct.Clav_Hotel
			, p.Clav_Agrupador
			, ra.Rango_Agrupador
			, ROW_NUMBER() 
					over (Partition BY hct.Clav_Hotel
						ORDER BY ra.Rango_Agrupador DESC )
				rango
		FROM (SELECT distinct Clav_Hotel, Clav_Plan FROM dbo.hoteles_cuartos_Tarifas2 with (nolock)) hct
			INNER JOIN @listaHoteles lh
				ON hct.Clav_Hotel = lh.Clav_Hotel
			INNER JOIN dbo.Planes p with (nolock)
				ON hct.Clav_Plan = p.Clav_Plan
			LEFT JOIN dbo.RM_Rangos_Agrupadores ra with (nolock)
				ON p.Clav_Agrupador = ra.Clav_Agrupador
		GROUP BY hct.Clav_Hotel
			, p.Clav_Agrupador
			, ra.Rango_Agrupador
		) t1
		INNER JOIN dbo.RM_Agrupadores_Servicios ac
			ON t1.Clav_Agrupador = ac.Clav_Agrupador
	WHERE t1.rango = 1
	ORDER BY t1.Clav_Hotel

	-- Insertamos los demás servicios (provenientes de Hoteles_Servicios) en @hot_serv
	INSERT INTO @hot_serv
	SELECT
		hs.Clav_Hotel,
		hs.Clav_Servicio
	FROM dbo.Hoteles_Servicios hs WITH (NOLOCK)
	-- Agregar esto cuando esté la tabla de servicios en cubo
	/*	INNER JOIN dbo.Servicios s WITH (NOLOCK)
			ON hs.Clav_Servicio = s.Clav_Servicio
	WHERE s.esCasa = 1
	--ORDER BY s.Orden
	*/

	-- Regresamos el resultado
	SELECT
		hs.Clav_Hotel
		, tcs.Codigo AS Categoria
		, count(tcs.Codigo) as Cantidad
	FROM @hot_serv hs
		INNER JOIN @listaHoteles lh					-- Sólo de los hoteles obtenidos en la lista
			ON hs.Clav_Hotel = lh.Clav_Hotel
		INNER JOIN Clasificaciones_Servicios cs WITH (NOLOCK)
			ON hs.Clav_Servicio = cs.Clav_Servicio
		INNER JOIN Tipos_Clasificaciones_Servicios tcs WITH (NOLOCK)
			ON cs.Clav_TipoClasificacionServicio = tcs.Clav_TipoClasificacionServicio
	WHERE tcs.Codigo <> 'ZIGNORE'					-- Categoria a ignorar
	GROUP BY hs.Clav_Hotel, tcs.Codigo
	ORDER BY hs.Clav_Hotel, tcs.Codigo

	-- END CODE -----------------------------------------------------------------------------
	
END
