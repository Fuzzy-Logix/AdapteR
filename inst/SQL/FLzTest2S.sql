SELECT 	FLzTest2S('Z_STAT',  a.GroupID, a.Num_Val, %numTails) AS Z_STAT,
 		FLzTest2S('P_VALUE', a.GroupID, a.Num_Val, %numTails) AS P_VALUE
FROM %tableName a;
 
 