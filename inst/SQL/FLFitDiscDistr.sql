WITH z (GroupID, NumOfTrials, NumOfSuccess) AS
 (
SELECT 1 AS GroupID, a.%num_trials, a.%num_success FROM %table_name AS a
)
 SELECT a.*
 FROM TABLE (FL%method%distributionUdt(z.GroupID, z.NumOfTrials, z.NumOfSuccess)
 HASH BY z.GroupID
 LOCAL ORDER BY z.GroupID, z.NumOfSuccess) AS a;