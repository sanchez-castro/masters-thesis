
DECLARE @fecha_ini AS CHAR(8) = '20130618'
--DECLARE @fecha_fin AS CHAR(8) = '20150617'    -- Quité la fecha final para que siempre tome hasta lo más nuevo.
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
EXEC spRM_ObtenerListaDeHoteles

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







