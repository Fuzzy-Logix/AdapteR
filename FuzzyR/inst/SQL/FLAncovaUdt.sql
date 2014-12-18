WITH tw (m_id, %variable, %control, %response, Significance)
AS (
		SELECT 1 AS m_id,
		%variable, 
		%control,
		%response,
		0.05 AS Significance  
		FROM %tableName t
		WHERE %whereClause
	)	
SELECT d.*
FROM TABLE (FLAncovaUdt (tw.m_id, tw.%variable, tw.%control, tw.%response, tw.Significance)
			HASH BY tw.m_id
			LOCAL ORDER BY tw.m_id
) AS d
ORDER BY 1