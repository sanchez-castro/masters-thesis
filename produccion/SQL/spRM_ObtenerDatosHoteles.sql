

-- =============================================

-- Author:     Felipe Gerard

-- Create date: 20150723

-- Description: Obtiene coordenadas y precios de los hoteles

-- Llamada:     EXEC spRM_ObtenerDatosHoteles

-- History:

-- Date           Author            Description

-- ---------------------------------------------

-- 

-- =============================================

/*CREATE*/ALTER PROCEDURE [dbo].[spRM_ObtenerDatosHoteles] 
@fecha_ini AS CHAR(8),
@country as CHAR(2)

AS

BEGIN

	-- SET NOCOUNT ON added to prevent extra result sets from

	-- interfering with SELECT statements.

 	SET NOCOUNT ON;

	-- BEGIN CODE -----------------------------------------------------------------------------
	
	DECLARE @listaHoteles AS TABLE (
		Clav_Hotel INT
	)
	DECLARE @precios AS TABLE (
		Clav_Hotel INT
		, Volumen_Noches INT
		, Precio_Dlls DOUBLE PRECISION
		, Utilidad_Dlls DOUBLE PRECISION
	)

	-- Lista de hoteles que queremos (por geografía, etc)
	INSERT INTO @listaHoteles
	EXEC spRM_ObtenerListaDeHoteles @country

	-- Precios de largo plazo de los hoteles
	INSERT INTO @precios
	SELECT
		h.Clav_Hotel
		, sum(rh.Noches) as Volumen_Noches
		, CASE
			WHEN sum(rh.Noches) = 0 THEN NULL
			ELSE sum(rh.Importe_Tot_Dlls)/sum(rh.Noches)
		END as Precio_Dlls
		, CASE
			WHEN sum(rh.Noches) = 0 THEN NULL
			ELSE  sum(rh.Utilidad_Dlls)/sum(rh.Noches)
		END as Utilidad_Dlls
	FROM dbo.Reservacion r with (nolock)
		INNER JOIN dbo.res_Hotel rh with (nolock)
			ON r.Clav_Res = rh.Clav_Res
		INNER JOIN dbo.Hoteles h with (nolock)
			ON rh.Clav_Hotel = h.Clav_Hotel
	WHERE R.fecha_venta >= @fecha_ini
	--and R.fecha_venta <= @fecha_fin
	and R.Email not like '%@bestday.com%'
	and R.Email not like '%@hoteldo.com%'
	and R.Email not like '%@e-travelsolution.com%'
	and R.status_reserva <> 'X'
	and R.status_pago='P'
	GROUP BY h.Clav_Hotel

	-- Pegar los precios a la lista de hoteles. Las coordenadas se pegan por separado porque hay hoteles con coordenadas pero sin precio
	SELECT
		lh.Clav_Hotel
		, h.longitude
		, h.latitude
		, p.Precio_Dlls
		, p.Volumen_Noches
		, p.Utilidad_Dlls
	FROM @listaHoteles lh
		LEFT JOIN @precios p
			ON lh.Clav_Hotel = p.Clav_Hotel
		LEFT JOIN dbo.Hoteles h with (nolock)
			ON lh.Clav_Hotel = h.Clav_Hotel
	ORDER BY Clav_Hotel

	-- END CODE -----------------------------------------------------------------------------

END