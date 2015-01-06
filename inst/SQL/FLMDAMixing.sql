SELECT HypothesisID, ClassID, SubClassID, Mixing 
FROM fzzlMDAMixing 
WHERE AnalysisID = '%analysisID' 
ORDER BY 1,2,3,4