SELECT HypothesisID, Iteration, LogLikelihood 
FROM fzzlMDALogLikelihood
WHERE AnalysisID = '%analysisID' 
ORDER BY 1,2,3