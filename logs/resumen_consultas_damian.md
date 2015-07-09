
SRH
--------------

* Tabla 1:
	+ Restaurantes, bares y antros
* Tabla 2:
	+ Actividades con nombres largos

SRH_temas
--------------

* ERROR

SRH_temas_infoH
---------------------

* Hotel (ID + nombre)
* Adult only?
* Características como texto

SRH_actividades
-------------------

* Hotel
* Servicio (ID + nombre)
* Cobran?
* OJO: Proviene de Hoteles_Servicios!!!!!

SRH_PuntosInteres
--------------------

* Algunos puntos de interés

SRH_servicios
-----------------

* Igual que actividades, más:
* Es actividad? Servicio? Instalación? Cuesta?


Bottom Line
----------------------

* Para el análisis:
	+ Lo ideal es la tabla de servicios, más:
	+ Datos sobre las cantidades. Por ejemplo lo de restaurantes se puede sacar de la tabla 1 generada en SRH a secas.
	+ No creo que sea práctico sacarlo del texto contenido en temas_infoH
* Para filtrar después:
	+ temas_infoH es muy útil. Según yo es como Hoteles.

* Notas adicionales:
	+ Servicios tiene el catálogo de qué significan los servicios, pero como temía está muy sucia. Las descripciones vienen en ING, ESP o POR, sin ningún distintivo.
	+ Servicios_Textos tiene los servicios con una descripción por idioma