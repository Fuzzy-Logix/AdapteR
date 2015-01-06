SELECT HypothesisID,Level,ClusterID,VarID,Centroid 
FROM fzzlKMeansDendrogram 
WHERE AnalysisID = '%analysisID'
ORDER BY 1,2,3,4