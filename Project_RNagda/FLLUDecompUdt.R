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

#Overloading lu
lu<-function(x, ...){
	UseMethod("lu", x)
}

lu.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("SELECT ",object@matrix_id_colname,", ",object@row_id_colname,", ",object@col_id_colname,", ",object@cell_val_colname," FROM  ",object@matrix_table," WHERE ",object@matrix_id_colname," = ",object@matrix_id_value,"")
	retobj<-sqlQuery(connection,sqlstr)
	data<-matrix(retobj[,4],nrow=sqrt(length(retobj$CELL_VAL)),ncol=sqrt(length(retobj$CELL_VAL)),byrow=TRUE)
	print(data)
	i<-1
	multiplier<-c()
	while(i<sqrt(length(retobj$CELL_VAL)))
	{	
		multiplier[i]<-(data[i+1,1]/data[i,i])
		i<-i+1
	}
	p<-1
	while(p<sqrt(length(retobj$CELL_VAL)))
	{
		for(q in 1:sqrt(length(retobj$CELL_VAL)))
		{
			data[p,q]<-multiplier[p]*data[p,q]
			data[p+1,q]<-(data[p+1,q]-data[p,q])
			
		}
		p<-p+1
	}
	data
	#data
	#data[i+1,]<-multiplier[i]*data[i+1,]
	#for(j in 1:sqrt(length(retobj$CELL_VAL)))
	#{
	#	newdata<-rbind(data[j,])
	#	j<-j+1
	#}
	#newdata
	#cat("'Matrix Factorization' of Formal class 'denseLU' [package Matrix] with",sqrt(length(retobj$CELL_VAL)),"slots\n") #"Matrix"
	#cat("..@x	: num[1:length(retobj$CELL_VAL)] ")

	
}


