Select  
	sum(power((tblUSArrests.Num_Val - a.valavg),2)) 
FROM 
	(SELECT VarID,average(tblUSArrests.Num_Val) as valavg FROM tblUSArrests Group By VarID) as a, 
	tblUSArrests 
WHERE 
	a.VarID = tblUSArrests.VarID;