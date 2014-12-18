WITH z (GroupID, NumOfTrials, NumOfSuccess) AS
 (
SELECT 1 AS GroupID, a.%s, a.%s FROM %s AS a
)
 SELECT a.*
 FROM TABLE (FL%s%sUdt(z.GroupID, z.NumOfTrials, z.NumOfSuccess)
 HASH BY z.GroupID
 LOCAL ORDER BY z.GroupID) AS a;