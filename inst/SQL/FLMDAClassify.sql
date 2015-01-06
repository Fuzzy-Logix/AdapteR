SELECT HypothesisID, ObsID, ClassID 
FROM fzzlMDAClassify 
WHERE AnalysisID = '%analysisID' 
ORDER BY 1,2,3