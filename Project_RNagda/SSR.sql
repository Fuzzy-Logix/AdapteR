SELECT 
	sum(power((a.valavg - fzzlKMeansDendrogram.Centroid),2))
	
FROM
	(SELECT VarID,average(tblUSArrests.Num_Val) as valavg FROM tblUSArrests Group By VarID) as a, 
	fzzlKMeansClusterID, 
	fzzlKMeansDendrogram
WHERE 
	fzzlKMeansDendrogram.AnalysisID = 'A479301' AND
	fzzlKMeansClusterID.AnalysisID = 'A479301' AND
	tblUSArrests.VarID=fzzlKMeansDendrogram.VarID AND
	fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID AND
	fzzlKMeansClusterID.ObsID = tblUSArrests.ObsID AND
	a.VarID = tblUSArrests.VarID