WITH z (GroupID, NumVal) AS
 (
SELECT 1 AS GroupID, a.%value FROM %table_name AS a
)
 SELECT a.*
 FROM TABLE (FL%method%distributionUdt(z.GroupID, z.NumVal)
 HASH BY z.GroupID
 LOCAL ORDER BY z.GroupID, z.NumVal) AS a;