
DECLARE @agrup_cat TABLE (
	Clav_Agrupador CHAR(2)
	, Categoria_Alimentos CHAR(20)
)

DECLARE @rangos_agrup TABLE (
	Clav_Agrupador CHAR(2)
	, Rango_Agrupador INT
)

DECLARE @hot_serv TABLE (
	Clav_Hotel INT
	, Clav_Servicio VARCHAR(20)
)

INSERT INTO @rangos_agrup VALUES
('AI', 4)
,('BB', 1)
,('RO', 0)
,('MD', 4)
,('SA', 4)
,('CP', 1)
,('DB', 1)
,('AG', 4)
,('FP', 3)
,('MP', 2)
,('BD', 2)
,('GA', 0)
,('ND', 0)

INSERT INTO @agrup_cat VALUES
('AI', 'BREAKFAST_INCLUDED')
,('AI', 'LUNCH_INCLUDED')
,('AI', 'DINNER_INCLUDED')
,('AI', 'BEVERAGES_INCLUDED')
,('BB', 'BREAKFAST_INCLUDED')
,('MD', 'BREAKFAST_INCLUDED')
,('MD', 'LUNCH_INCLUDED')
,('MD', 'DINNER_INCLUDED')
,('MD', 'BEVERAGES_INCLUDED')
,('SA', 'BREAKFAST_INCLUDED')
,('SA', 'LUNCH_INCLUDED')
,('SA', 'DINNER_INCLUDED')
,('SA', 'BEVERAGES_INCLUDED')
,('CP', 'BREAKFAST_INCLUDED')
,('DB', 'BREAKFAST_INCLUDED')
,('AG', 'BREAKFAST_INCLUDED')
,('AG', 'LUNCH_INCLUDED')
,('AG', 'DINNER_INCLUDED')
,('AG', 'BEVERAGES_INCLUDED')
,('FP', 'BREAKFAST_INCLUDED')
,('FP', 'LUNCH_INCLUDED')
,('FP', 'DINNER_INCLUDED')
,('MP', 'BREAKFAST_INCLUDED')
,('MP', 'DINNER_INCLUDED')
,('BD', 'BREAKFAST_INCLUDED')
,('BD', 'DINNER_INCLUDED')

--select * from @agrup_cat
--select * from @rangos_agrup

INSERT INTO @hot_serv
SELECT
	Clav_Hotel
	, Categoria_Alimentos AS Clav_Servicio
FROM (
	SELECT
		hct.Clav_Hotel
		, p.Clav_Agrupador
		, pa.Nombre_Agrupador
		, ra.Rango_Agrupador
		, ROW_NUMBER() 
				over (Partition BY hct.Clav_Hotel
					ORDER BY ra.Rango_Agrupador DESC )
			rango
	FROM Matrix_Reloaded.dbo.hoteles_cuartos_Tarifas2 hct with (nolock)
		INNER JOIN Matrix_Reloaded.dbo.Planes p with (nolock)
			ON hct.Clav_Plan = p.Clav_Plan
		INNER JOIN Matrix_Reloaded.dbo.Planes_Agrupadores pa with (nolock)
			ON p.Clav_Agrupador = pa.Clav_Agrupador
		LEFT JOIN @rangos_agrup ra
			ON pa.Clav_Agrupador = ra.Clav_Agrupador
	GROUP BY hct.Clav_Hotel
		, p.Clav_Agrupador
		, pa.Nombre_Agrupador
		, ra.Rango_Agrupador
	) t1
	INNER JOIN @agrup_cat ac
		ON t1.Clav_Agrupador = ac.Clav_Agrupador
WHERE t1.rango = 1
ORDER BY t1.Clav_Hotel


---------------------------------------------------
-- Los demás servicios

INSERT INTO @hot_serv
SELECT
	hs.Clav_Hotel,
	hs.Clav_Servicio
FROM Matrix_Reloaded.dbo.Hoteles_Servicios hs WITH (NOLOCK)
	INNER JOIN Matrix_Reloaded.dbo.Servicios s WITH (NOLOCK)
		ON hs.Clav_Servicio = s.Clav_Servicio
WHERE s.esCasa = 1
ORDER BY s.Orden


SELECT *
FROM @hot_serv
ORDER BY Clav_Hotel, Clav_Servicio








