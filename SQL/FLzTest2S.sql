SELECT FLzTest2S('Z_STAT', a.GroupID, a.Num_Val, %s) AS Z_STAT,
 FLzTest2S('P_VALUE', a.GroupID, a.Num_Val, %s) AS P_VALUE
 FROM %s a;
 
 