REPLACE PROCEDURE WorkaroundDecisionTree (	IN TableName VARCHAR(100), 
											IN ObsIDColName VARCHAR(100),
											IN VarIDColName VARCHAR(100),
											IN ValueColName VARCHAR(100),
											IN MinObsforParent INTEGER,
											IN MaxLevel INTEGER,
											IN PurityThreshold DOUBLE PRECISION,								
											IN Note VARCHAR(255) )
DYNAMIC RESULT SETS 1
BEGIN
	DECLARE AnalysisID VARCHAR(30);
	DECLARE statement1_str VARCHAR(500);
	DECLARE result_set CURSOR WITH RETURN ONLY FOR stmt1;
	
	CALL FLDecisionTreeMN(TableName,ObsIDColName,VarIDColName,ValueColName,MinObsforParent,MaxLevel,PurityThreshold,Note,AnalysisID);    
	SET statement1_str = 'SELECT AnalysisID FROM fzzlDecisionTreeMNInfo WHERE AnalysisID = ?';
	PREPARE stmt1 FROM statement1_str;
	OPEN result_set USING AnalysisID;
	DEALLOCATE PREPARE stmt1;
END;