#overloading det
det <- function (x, ...){
	UseMethod("det", x)
}

det.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS (SELECT a.",object@matrix_id_colname,", a.",object@row_id_colname,", a.",object@col_id_colname,", a.",object@cell_val_colname," FROM  ",object@matrix_table," a WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") SELECT a.* FROM TABLE (FLMatrixDetUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) HASH BY z.Matrix_ID LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a ORDER BY 1;")
	retobj<-sqlQuery(connection,sqlstr)
	result<-retobj$OutputDetVal
	result
}