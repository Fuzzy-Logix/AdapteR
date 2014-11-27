WITH tw (m_id, %category, %response)
AS (
		SELECT 1 AS m_id,
		%category,
		%response
		FROM %table t
		WHERE %whereClause
	)
SELECT d.*
FROM TABLE (FLAnova1WayUdt (tw.m_id, tw.%category, tw.%response)
			HASH BY tw.m_id
			LOCAL ORDER BY tw.m_id
) AS d
ORDER BY 1
