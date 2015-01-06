SELECT 	FLtTest2S('T_STAT', '%varType', a.GroupID, a.Num_Val, %numTails)  AS T_STAT,
		FLtTest2S('P_VALUE', '%varType', a.GroupID, a.Num_Val, %numTails) AS P_VALUE
FROM %tableName a;
 
