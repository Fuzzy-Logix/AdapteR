WITH tw (m_id, %s, %s, %s, Significance)
AS (
		SELECT 1 AS m_id,
		%s, 
		%s,
		%s,
		0.05 AS Significance  
		FROM %s t
		WHERE %s
	)	
SELECT d.*
FROM TABLE (FLAncovaUdt (tw.m_id, tw.%s, tw.%s, tw.%s, tw.Significance)
			HASH BY tw.m_id
			LOCAL ORDER BY tw.m_id
) AS d
ORDER BY 1