SELECT HypothesisID, ObsID, ClassID, SubclassID, Weight 
FROM fzzlMDAWeight 
WHERE AnalysisID = '%analysisID' 
ORDER BY 1,2,3,4,5