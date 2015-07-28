
-- Crear la tabla
CREATE TABLE RM_Hoteles_HotelesRecomendaciones (
	Clav_Hotel INT
	, Clav_HotelRecomendacion INT
	, Prioridad INT
	, Fecha_Actualizacion DATETIME
)

-- Índice para acelerar búsquedas
USE [Matrix_Estadisticas]
GO
CREATE NONCLUSTERED INDEX [IX_RM_Hoteles_HotelesRecomendaciones_Clav_Hotel] ON [dbo].[RM_Hoteles_HotelesRecomendaciones]
(
	[Clav_Hotel] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
GO

