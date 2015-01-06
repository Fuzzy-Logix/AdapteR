WITH z (GroupID, NumVal) AS
 (
SELECT 1 AS GroupID, a.%s FROM %s AS a
)
 SELECT a.*
 FROM TABLE (FL%s%sUdt(z.GroupID, z.NumVal)
 HASH BY z.GroupID
 LOCAL ORDER BY z.GroupID) AS a;