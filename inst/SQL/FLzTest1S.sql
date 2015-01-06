SELECT 	FLzTest1S('Z_STAT',  %mu, a.%input1, %numTails) AS Z_STAT,
 		FLzTest1S('P_VALUE', %mu, a.%input1, %numTails) AS P_VALUE
 FROM %tableName a;
 
