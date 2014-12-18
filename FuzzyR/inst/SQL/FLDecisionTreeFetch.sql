SELECT TreeLevel, NodeID, ParentNodeID, IsLeaf, SplitVarID, SplitVal, ChildNodeLeft, ChildNodeRight, NodeSize, PredictClass, PredictClassProb 
FROM fzzlDecisionTreeMN 
WHERE AnalysisID = '%analysisID'
ORDER BY 1,2,3,4