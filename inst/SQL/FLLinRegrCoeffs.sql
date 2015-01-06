SELECT b.MODELID,b.COEFFID,a.VAR_TYPE,a.COLUMN_NAME,a.CATVALUE,b.COEFFVALUE,b.STDERR,b.TSTAT,b.PVALUE,b.NONZERODENSITY,b.CORRELWITHRES
FROM fzzlRegrDataPrepMap a,fzzlLinRegrCoeffs b 
WHERE 	a.AnalysisID = '%wideToDeepAnalysisID' 
		AND b.AnalysisID = '%analysisID' 
		AND a.Final_VarID = b.COEFFID 
		AND b.MODELID = 
		(
			SELECT MAX(MODELID)
			FROM fzzllinregrcoeffs
			WHERE AnalysisID = '%analysisID'
		)
ORDER BY b.COEFFID