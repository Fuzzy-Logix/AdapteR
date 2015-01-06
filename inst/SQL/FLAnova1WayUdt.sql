WITH tw (m_id, %variable, %response)
AS (
		SELECT 1 AS m_id,
		%variable,
		%response
		FROM %tableName t
		WHERE %whereClause
	)
SELECT d.*
FROM TABLE (FLAnova1WayUdt (tw.m_id, tw.%variable, tw.%response)
			HASH BY tw.m_id
			LOCAL ORDER BY tw.m_id
) AS d
ORDER BY 1
