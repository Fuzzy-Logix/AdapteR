setOldClass("RODBC") 
library(Matrix)
setClass(
	"FLMatrix", 
	slots = list(	
		odbc_connection = "RODBC", 
		db_name = "character", 
		matrix_table = "character",
		matrix_id_value	= "numeric",
		matrix_id_colname = "character",
		row_id_colname = "character",
		col_id_colname = "character",
		cell_val_colname = "character"
	)
)

FLMatrix <- function(connection, database, matrix_table, matrix_id_value, 
					matrix_id_colname = "MATRIX_ID", row_id_colname = "ROW_ID", col_id_colname = "COL_ID", cell_val_colname = "CELL_VAL") 

{
	sqlQuery(connection, paste("DATABASE", database))
	sqlQuery(connection, "SET ROLE ALL")
	
	new("FLMatrix", odbc_connection = connection, db_name = database, matrix_table = matrix_table, matrix_id_value = matrix_id_value, matrix_id_colname = matrix_id_colname, row_id_colname = row_id_colname, col_id_colname = col_id_colname, cell_val_colname = cell_val_colname)
}

#Overloading t
t<-function(x, ...){
	UseMethod("t", x)
}

t.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("SELECT ",object@matrix_id_colname,", ",object@row_id_colname,", ",object@col_id_colname,", ",object@cell_val_colname," FROM  ",object@matrix_table," WHERE ",object@matrix_id_colname," = ",object@matrix_id_value,"")
	retobj<-sqlQuery(connection,sqlstr)
	colMax<-function(column){
	sapply(column, max, na.rm = TRUE)
	}
	nrow=colMax(object@row_id_colname)
	ncol=colMax(object@col_id_colname)
	
}
	#data<-matrix(retobj[,4],nrow,ncol,byrow=TRUE)
	#List<-list()
	#for(i in 1:nrow)
	#{
	#transpose<-data[,i]
	#List[[i]]<-transpose					
	#}	
	#newdata<-do.call(rbind, List)
	#newdata
#}

