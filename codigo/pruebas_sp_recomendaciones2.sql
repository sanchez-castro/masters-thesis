
SELECT *
FROM Hoteles_Busquedas_Recomendaciones

SELECT *
FROM RM_Hoteles_HotelesRecomendaciones
WHERE Clav_Hotel = 23
ORDER BY Prioridad

-------------

SELECT Fecha_Actualizacion, count(Fecha_Actualizacion) AS N
FROM RM_Hoteles_HotelesRecomendaciones
GROUP BY Fecha_Actualizacion
ORDER BY Fecha_Actualizacion

--------------

SELECT count(distinct Clav_Hotel)
FROM RM_Hoteles_HotelesRecomendaciones

-----------------

EXEC spRM_ObtenerRecomendacionesHoteles @clav_hotel = 3901, @fecha = '2015-08-01'


----------------

SELECT TOP 1000 *
FROM Hoteles_Servicios WITH(NOLOCK)
WHERE Clav_Hotel = 2 OR Clav_Hotel > 5000


SELECT *
FROM RM_Hoteles_HotelesRecomendaciones
ORDER BY Clav_Hotel, Prioridad




SELECT TOP 100 *
FROM Hoteles