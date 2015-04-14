SELECT b.VarID, a.VAR_TYPE, a.COLUMN_NAME, a.CATVALUE, b.ClassValue, b.VarValue, b.ClassVarCount
FROM fzzlRegrDataPrepMap a, fzzlNaiveBayesModel b 
WHERE b.AnalysisID = '%analysisID'
  AND a.AnalysisID = '%wideToDeepAnalysisID'
  AND a.Final_VarID = b.VarID
ORDER BY 1,5,6;