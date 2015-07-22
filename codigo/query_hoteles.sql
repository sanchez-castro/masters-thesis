
DECLARE @fecha_ini AS CHAR(8) = '20130618'
DECLARE @fecha_fin AS CHAR(8) = '20150617'

SELECT
	h.Clav_Hotel
	, h.longitude
	, h.latitude
	, sum(rh.Noches) as Volumen_Noches
	, CASE
		WHEN sum(rh.Noches) = 0 THEN NULL
		ELSE sum(rh.Importe_Tot_Dlls)/sum(rh.Noches)
	END as Precio_Dlls
	, CASE
		WHEN sum(rh.Noches) = 0 THEN NULL
		ELSE  sum(rh.Utilidad_Dlls)/sum(rh.Noches)
	END as Utilidad_Dlls
FROM Matrix_Reloaded.dbo.Reservacion r with (nolock)
	INNER JOIN Matrix_Reloaded.dbo.res_Hotel rh with (nolock)
		ON r.Clav_Res = rh.Clav_Res
	INNER JOIN Matrix_Reloaded.dbo.Hoteles h with (nolock)
		ON rh.Clav_Hotel = h.Clav_Hotel
WHERE R.fecha_venta >= @fecha_ini
and R.fecha_venta <= @fecha_fin
and R.Email not like '%@bestday.com%'
and R.Email not like '%@hoteldo.com%'
and R.Email not like '%@e-travelsolution.com%'
and R.status_reserva <> 'X'
and R.status_pago='P'
GROUP BY h.Clav_Hotel, h.longitude, h.latitude
ORDER BY h.Clav_Hotel, h.longitude, h.latitude




