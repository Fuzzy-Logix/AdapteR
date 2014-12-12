SELECT ObsID, ObservedClass, NodeID, PredictedClass, PredictClassProb
FROM fzzlDecisionTreeMNPred
WHERE AnalysisID = '%analysisID'
ORDER BY 1,2,3,4