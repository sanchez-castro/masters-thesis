Modelo de recomendación de hoteles
========================================================
author: Revenue Management. Preparado por Felipe Gerard
date: 25 de junio de 2015




Antecedentes
========================================================

- **Objetivo:** Recomendar hoteles similares a uno dado
- Solución actual: búsqueda por destino y estrellas
- **Problemas:**
  - No se toma en cuenta la filosofía del hotel
  - No se aprovecha toda la información disponible
  - El criterio es más o menos estático
- **Ideas:**
  - Tomar en cuenta el perfil de los hoteles
  - Incorporar información geográfica
  - Hacer que el criterio sea dinámico


Plan de ataque
========================================================

1. Obtener información relevante
  - Servicios
  - Geolocalización
  - Precios recientes
2. Caracterizar los hoteles
  - Categorías de servicios equivalentes
3. Generar un criterio integral de similitud
  - Cercanía
  - Precio
  - Al menos los mismos servicios
  - Perfil similar

Información
========================================================

- Nos la saltaremos...


Caracterización de hoteles
========================================================

- Usar servicios es miope
- No toma en cuenta los sustitutos
- Agrupar servicios manualmente con modelo de apoyo:

![Aglomerado jerárquico](graficas/dendrograma_parcial.png)


Precio y cercanía
========================================================

- **Precio:**
  - Favorecer precios bajos
  - Porcentaje máximo arriba del hotel original
- **Cercanía:**
  - Criterio dinámico
  - Depende de densidad de hoteles
  - Evita enmascaramiento por hoteles distintos
  - Gráfica


Cercanía
========================================================

![a](graficas/cercania.png)




Similaridad
========================================================

![plot of chunk unnamed-chunk-3](presentacion_v1-figure/unnamed-chunk-3-1.png) 


Similaridad
========================================================


![plot of chunk unnamed-chunk-4](presentacion_v1-figure/unnamed-chunk-4-1.png) 

Comentarios finales
========================================================

- La base de servicios _no está homologada_ para los demás países
- El modelo actual sólo está hecho con los datos de México
- Falta por definir algunos detalles del modelo
- Demo










