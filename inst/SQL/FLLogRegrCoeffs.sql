SELECT b.COEFFID, a.VAR_TYPE, a.COLUMN_NAME, b.COEFFVALUE, b.STDERR, b.CHISQ, b.PVALUE
FROM fzzlRegrDataPrepMap a,fzzlLogRegrCoeffs b 
WHERE a.AnalysisID = '%wideToDeepAnalysisID' AND b.AnalysisID ='%analysisID' AND a.Final_VarID = b.COEFFID
ORDER BY b.COEFFID