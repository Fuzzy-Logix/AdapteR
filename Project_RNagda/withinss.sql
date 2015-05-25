SELECT 
	sum(power((tblUSArrests.Num_Val - fzzlKMeansDendrogram.Centroid ),2))
FROM 
	fzzlKMeansClusterID,
	tblUSArrests,
	fzzlKMeansDendrogram 
WHERE 
	fzzlKMeansDendrogram.AnalysisID = 'A479301' AND
	fzzlKMeansClusterID.AnalysisID = 'A479301' AND
	tblUSArrests.VarID=fzzlKMeansDendrogram.VarID AND
	fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID AND
	fzzlKMeansClusterID.ObsID = tblUSArrests.ObsID

SELECT 
	sum(power((tblIrisDeep.Num_Val - fzzlKMeansDendrogram.Centroid ),2))
FROM 
	fzzlKMeansClusterID,
	tblIrisDeep,
	fzzlKMeansDendrogram 
WHERE 
	fzzlKMeansDendrogram.AnalysisID = 'A233900' AND
	fzzlKMeansClusterID.AnalysisID = 'A233900' AND
	tblIrisDeep.VarID=fzzlKMeansDendrogram.VarID AND
	fzzlKMeansClusterID.ClusterID = fzzlKMeansDendrogram.ClusterID AND
	fzzlKMeansClusterID.ObsID = tblIrisDeep.ObsID AND
	tblIrisDeep.VarID IN (1,2,3,4)