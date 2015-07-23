/*
Missing Index Details from query_hoteles_servicios_syscubo.sql - syscubo.Matrix_Estadisticas (bmxddt005062 (87))
The Query Processor estimates that implementing the following index could improve the query cost by 13.8096%.
*/

-- Hoteles

USE [Matrix_Estadisticas]
GO
CREATE NONCLUSTERED INDEX [ix_Hoteles_Internet_Activo_Clav_Pais_latitude_longitude]
ON [dbo].[Hoteles] ([Internet],[Activo],[Clav_Pais],[latitude],[longitude])
INCLUDE ([Clav_Hotel])
GO

-- Hoteles - servicios
USE [Matrix_Estadisticas]
GO
CREATE NONCLUSTERED INDEX [ix_hoteles_cuartos_Tarifas2_Clav_Plan]
ON [dbo].[hoteles_cuartos_Tarifas2] ([Clav_Plan])
INCLUDE ([Clav_Hotel])
GO
