SELECT HypothesisID, ClassID, SubclassID, VarID, Mu 
FROM fzzlMDAMu 
WHERE AnalysisID = '%analysisID' 
ORDER BY 1,2,3,4