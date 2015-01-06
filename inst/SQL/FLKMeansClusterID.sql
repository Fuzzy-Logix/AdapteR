SELECT HypothesisID,ObsID,ClusterID 
FROM fzzlKMeansClusterID 
WHERE AnalysisID = '%analysisID'
ORDER BY 1,2,3