
actualizar_info.R
correr_modelo.R
preparar_resultado[_version_rapida].R
actualizar_bd.R


NOTAS:
------------

* Para producción, hay que cambiar el nombre de la tabla *RM_PruebaHotelesRecomendaciones* al nombre bueno.
* Falta crear el índice de la tabla de recomendaciones y ponerlo en el SP
* La columna de recomendaciones Clav_HotelBusquedaRecomendacion **NO** puede ser PRIMARY KEY porque entonces truena al hacer las inserciones.

