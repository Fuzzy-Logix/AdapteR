CREATE TABLE %outTable AS ( 
	WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS
	(
		SELECT a.%matrixID,
		a.%rowID,
		a.%columnID,
		a.%cellValue
		 FROM %matrixTable a
		 WHERE a.%matrixID = %matrixValue
	)
	 SELECT a.*
	 FROM TABLE (
		FLSVDUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val)
		HASH BY z.Matrix_ID
		LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
	) AS a
) WITH DATA;