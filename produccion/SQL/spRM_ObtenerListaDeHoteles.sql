

-- =============================================

-- Author:     Felipe Gerard

-- Create date: 20150722

-- Description: Obtiene lista de hoteles para alimentar al Sistema de Recomendación de Hoteles

-- Llamada:     EXEC spRM_ObtenerListaDeHoteles 'MX'

-- History:

-- Date           Author            Description

-- ---------------------------------------------

-- 

-- =============================================

/*CREATE*/ ALTER PROCEDURE [dbo].[spRM_ObtenerListaDeHoteles] 
@country as CHAR(2)
	

AS

BEGIN

	-- SET NOCOUNT ON added to prevent extra result sets from

	-- interfering with SELECT statements.

 	SET NOCOUNT ON;

		SELECT distinct hc.Clav_Hotel
			FROM Hoteles_Contratos hc with (nolock)
			INNER JOIN Hoteles h with (nolock)
				ON hc.Clav_Hotel = h.Clav_Hotel
			INNER JOIN Hoteles_Servicios hs with (nolock)
				ON hc.Clav_Hotel = hs.Clav_hotel
			WHERE h.Clav_Pais = @country 
			/*h.Clav_Pais in ('AR','MX','BR','US')*/
		/*(
			h.Clav_Pais in ('AR','MX')
			OR (h.Clav_Pais = 'BR' AND hc.tipo_notif in ('A','E','H','K'))
			OR (h.Clav_Pais = 'US' AND hc.tipo_notif in ('A','E','H'))
		)*/
		AND h.Activo = 1  
		AND h.Internet = 1
		AND h.latitude IS NOT NULL
		AND h.longitude IS NOT NULL
	ORDER BY hc.Clav_Hotel

END