SELECT 	FLtTest1S('T_STAT', %mu, a.%input1, %numTails)  AS T_STAT, 
		FLtTest1S('P_VALUE', %mu, a.%input1, %numTails) AS P_VALUE
FROM %tableName a;

