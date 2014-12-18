SELECT b.VarID, a.VAR_TYPE, a.COLUMN_NAME, a.CATVALUE, b.RSquared, b.VIF 
FROM fzzlRegrDataPrepMap a,fzzlvifstats b 
WHERE a.AnalysisID = '%wideToDeepAnalysisID' AND b.AnalysisID = '%analysisID' AND a.Final_VarID = b.VarID 
ORDER BY b.VarID