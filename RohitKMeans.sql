REPLACE PROCEDURE RohitKMeans (	IN TableName VARCHAR(100),
								IN ObsIDColName VARCHAR(100),
								IN VarIDColName VARCHAR(100),
								IN ValueColName VARCHAR(100),
								IN WhereClause VARCHAR(512),
								IN Clusters INTEGER,
								IN Iterations INTEGER,								
								IN Hypothesis INTEGER,
								IN Note VARCHAR(255) )
DYNAMIC RESULT SETS 1
BEGIN
	DECLARE AnalysisID VARCHAR(30);
	DECLARE statement1_str VARCHAR(500);
	DECLARE result_set CURSOR WITH RETURN ONLY FOR stmt1;
	
	CALL FLKMeans(TableName,ObsIDColName,VarIDColName,ValueColName,WhereClause,Clusters,Iterations,Hypothesis,Note,AnalysisID);    
	SET statement1_str = 'SELECT Analysisid FROM fzzlKMeansInfo WHERE Analysisid = ?';
	PREPARE stmt1 FROM statement1_str;
	OPEN result_set USING AnalysisID;
	DEALLOCATE PREPARE stmt1;
END;