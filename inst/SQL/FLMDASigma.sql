SELECT HypothesisID, VarID1, VarID2, Sigma 
FROM fzzlMDASigma 
WHERE AnalysisID = '%analysisID' 
ORDER BY 1,2,3,4