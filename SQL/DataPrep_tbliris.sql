DROP TABLE tblirisDeep;
CALL FLRegrDataPrep('tbliris',-- Input wide table
'ObsID',-- Name of the ObsID Column
'Species',-- Name of the dependent variable
'tblirisdeep',-- Name of the output deep table
'ObsID',-- Name of output ObsID column
'VarID',-- Name of output VarID column
'Num_Value',-- Name of output Value column
0,--Transform categorical to dummy variables
0,--Perform standardization
0,--Perform variable reduction
0,--Make data sparse
0,--Minimum acceptable Standard Deviation
0,--Maximum acceptable correlation
0,--0=>Training data set,1=>Test data set
'Petal_Length,Petal_Width',--Columns to exclude from conversion
NULL,-- Class Specification
NULL,-- WHERE clause
NULL,-- Provided in case of a re-run,else NULL
AnalysisID);

DELETE FROM tblirisdeep
WHERE Num_Value IS NULL 

DELETE FROM tblirisdeep
WHERE VarID IN (0,-1) 

INSERT INTO tblirisdeep 
SELECT DISTINCT a.ObsID,
		-1 AS VarID,
		1.0 AS Num_Value
FROM tblirisdeep a,
	 tbliris b
WHERE a.ObsID = b.ObsID AND b.Species = 'setosa'

UNION ALL

SELECT 	DISTINCT a.ObsID,
		-1 AS VarID,
		2.0 AS Num_Value
FROM tblirisdeep a,
	 tbliris b
WHERE a.ObsID = b.ObsID AND b.Species = 'versicolor'

UNION ALL

SELECT 	DISTINCT a.ObsID,
		-1 AS VarID,
		3.0 AS Num_Value
FROM tblirisdeep a,
	 tbliris b
WHERE a.ObsID = b.ObsID AND b.Species = 'virginica';