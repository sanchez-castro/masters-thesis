DECLARE @agrup_cat TABLE (
	Clav_Agrupador CHAR(2)
	, Categoria_Alimentos CHAR(4)
	, BREAKFAST_INCLUDED BIT
	, LUNCH_INCLUDED BIT
	, DINNER_INCLUDED BIT
	, BEVERAGES_INCLUDED BIT
	, RANGO_PLAN INT
)

DECLARE @serv TABLE (
	Clav_Servicio CHAR(20)
)

DECLARE @hot_serv TABLE (
	Clav_Hotel INT
	, Clav_Servicio VARCHAR(20)
)

INSERT INTO @serv VALUES
('BREAKFAST_INCLUDED')
,('LUNCH_INCLUDED')
,('DINNER_INCLUDED')
,('BEVERAGES_INCLUDED')

INSERT INTO @agrup_cat VALUES
('AI', 'BLDR', 1, 1, 1, 1, 4)
,('BB', 'B---', 1, 0, 0, 0, 1)
,('RO', '----', 0, 0, 0, 0, 0)
,('MD', 'BLDR', 1, 1, 1, 1, 4)
,('SA', 'BLDR', 1, 1, 1, 1, 4)
,('CP', 'B---', 1, 0, 0, 0, 1)
,('DB', 'B---', 1, 0, 0, 0, 1)
,('AG', 'BLDR', 1, 1, 1, 1, 4)
,('FP', 'BLD-', 1, 1, 1, 0, 3)
,('MP', 'B-D-', 1, 0, 1, 0, 2)
,('BD', 'B-D-', 1, 0, 1, 0, 2)
,('GA', '----', 0, 0, 0, 0, 0)
,('ND', '----', 0, 0, 0, 0, 0)


--SELECT *
--FROM @serv

INSERT INTO @hot_serv
SELECT
	t2.Clav_Hotel,
	t2.Clav_Servicio
FROM
	(
	SELECT
		t.*
		, s.Clav_Servicio
		, CASE
			WHEN s.Clav_Servicio = 'BREAKFAST_INCLUDED' THEN BREAKFAST_INCLUDED
			WHEN s.Clav_Servicio = 'LUNCH_INCLUDED' THEN LUNCH_INCLUDED
			WHEN s.Clav_Servicio = 'DINNER_INCLUDED' THEN DINNER_INCLUDED
			WHEN s.Clav_Servicio = 'BEVERAGES_INCLUDED' THEN BEVERAGES_INCLUDED
		END ind
	FROM
		(
		SELECT
			hct.Clav_Hotel
			, p.Clav_Agrupador
			, pa.Nombre_Agrupador
			, BREAKFAST_INCLUDED
			, LUNCH_INCLUDED
			, DINNER_INCLUDED
			, BEVERAGES_INCLUDED
			, RANGO_PLAN
			, ROW_NUMBER() 
					over (Partition BY hct.Clav_Hotel
						ORDER BY RANGO_PLAN DESC )
				rango
		FROM Matrix_Reloaded.dbo.hoteles_cuartos_Tarifas2 hct with (nolock)
			INNER JOIN Matrix_Reloaded.dbo.Planes p with (nolock)
				ON hct.Clav_Plan = p.Clav_Plan
			INNER JOIN Matrix_Reloaded.dbo.Planes_Agrupadores pa with (nolock)
				ON p.Clav_Agrupador = pa.Clav_Agrupador
			INNER JOIN @agrup_cat ac
				ON pa.Clav_Agrupador = ac.Clav_Agrupador
		GROUP BY hct.Clav_Hotel
			, p.Clav_Agrupador
			, pa.Nombre_Agrupador
			, BREAKFAST_INCLUDED
			, LUNCH_INCLUDED
			, DINNER_INCLUDED
			, BEVERAGES_INCLUDED
			, RANGO_PLAN
		) t
		, @serv s
	WHERE t.rango = 1
	) t2
WHERE t2.ind = 1


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







