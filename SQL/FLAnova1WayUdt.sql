WITH tw (m_id, %s, %s)
AS (
		SELECT 1 AS m_id,
		%s, 
		%s  
		FROM %s t
		WHERE %s
	)	
SELECT d.*
FROM TABLE (FLAnova1WayUdt (tw.m_id, tw.%s, tw.%s)
			HASH BY tw.m_id
			LOCAL ORDER BY tw.m_id
) AS d
ORDER BY 1