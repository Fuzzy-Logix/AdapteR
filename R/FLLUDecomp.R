#' @include utilities.R
#' @include FLMatrix.R
#' @include FLSparseMatrix.R
#' @include FLVector.R
#' @include FLPrint.R
#' @include FLIs.R
#' @include FLDims.R
NULL

library(Matrix)

#' An S4 class to represent LU Decomposition
#' @slot x object of class FLVector
#' @slot perm object of class FLVector
#' @slot Dim object of class FLVector
#' @slot lower object of class FLMatrix
#' @slot upper object of class FLMatrix
#' @slot data_perm object of class FLMatrix
setClass(
	"FLLU",
	slots=list(
		x="FLVector",
		perm="FLVector",
		Dim="vector",
		lower="FLMatrix",
		upper="FLMatrix",
		data_perm="FLMatrix"
	)
)



# #' An S4 class to represent L,U and P factors as a list of matrices
# #' @slot luobject object of class FLLU
# setClass(
# 	"expandFLLU",
# 	slots=list(
# 		luobject="FLLU"
# 	)
# )


lu<-function(x, ...){
	UseMethod("lu",x)
}

lu.default <- Matrix::lu

#' LU Decomposition.
#'
#' The LU decomposition involves factorizing a matrix as the product of a lower
#' triangular matrix L and an upper triangular matrix U. Permutation matrix is also provided in the output.
#' If permutation matrix is not used in the decomposition, the output of permutation matrix is an identity matrix.
#'
#' \code{lu} replicates the equivalent lu() generic function.\cr
#' The wrapper overloads lu and implicitly calls FLLUDecompUdt.\cr\cr
#' \code{expand} decomposes the compact form to a list of matrix factors.\cr
#' The expand method returns L,U and P factors as a list of FLMatrices.\cr
#'
#' The decomposition is of the form A = P L U where typically all matrices are of size (n x n),
#' and the matrix P is a permutation matrix, L is lower triangular and U is upper triangular.
#' @method lu FLMatrix
#' @param object is of class FLMatrix
#' @section Constraints:
#' Input can only be with maximum dimension limitations
#' of (1000 x 1000).
#' @return
#' \item{x}{the FLVector form of "L" (unit lower triangular) and "U" (upper triangular) factors of the original matrix}
#' \item{perm}{FLVector that describes the permutation applied to the rows of the original matrix}
#' \item{Dim}{FLVector that gives the dimension of the original matrix}
#' \item{lower}{FLMatrix representing the lower triangular matrix}
#' \item{upper}{FLMatrix representing the upper triangular matrix}
#' \item{data_perm}{FLMatrix representing the permutation matrix}
#' @examples
#' connection<-odbcConnect("Gandalf")
#' flmatrix <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 5)
#' FLLUobject <- lu(flmatrix)
#' expand(FLLUobject)
#' expand(lu(flmatrix))$L
#' expand(lu(flmatrix))$U
#' expand(lu(flmatrix))$P
#' @export

lu.FLMatrix<-function(object)
{
	connection<-getConnection(object)
	flag3Check(connection)
	flag1Check(connection)
	
	tempResultTable <- gen_unique_table_name("tblLUDecompResult")
	tempDecompTableVector <<- c(tempDecompTableVector,tempResultTable)

    sqlstr0 <- paste0("CREATE TABLE ",getRemoteTableName(result_db_name,tempResultTable)," AS(",
    				 viewSelectMatrix(object, "a","z"),
                     outputSelectMatrix("FLLUDecompUdt",viewName="z",localName="a",
                    	outColNames=list("OutputMatrixID","OutputRowNum",
                    		"OutputColNum","OutputValL","OutputValU","OutputPermut"),
                    	whereClause=") WITH DATA;")
                   )

    sqlSendUpdate(connection,sqlstr0)

	# calculating LU matrix
	MID1 <- max_matrix_id_value
    max_matrix_id_value <<- max_matrix_id_value + 1

	sqlstrLU1 <-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
					" SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,CAST(OutputValL AS NUMBER) 
					  FROM ",remoteTable(result_db_name,tempResultTable),
					 " WHERE OutputRowNum > OutputColNum 
				   AND OutputValL IS NOT NULL;")

	sqlstrLU2 <-paste0("INSERT INTO ",
					getRemoteTableName(result_db_name,result_matrix_table),
					" SELECT ",MID1,
					         ",OutputRowNum
					          ,OutputColNum
					          ,CAST(OutputValU AS NUMBER) 
					  FROM ",remoteTable(result_db_name,tempResultTable),
					 " WHERE OutputRowNum <= OutputColNum 
				   AND OutputValU IS NOT NULL;")

	# calculating Permutation FLMatrix
    data_perm <- FLMatrix( 
			       connection = connection, 
			       database = result_db_name, 
			       matrix_table = tempResultTable, 
				   matrix_id_value = "",
				   matrix_id_colname = "", 
				   row_id_colname = "OutputRowNum", 
				   col_id_colname = "OutputColNum", 
				   cell_val_colname = "OutputPermut",
				   whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutputPermut IS NOT NULL "))


	# calculating l FLmatrix
    l<-FLMatrix( 
	       connection = connection, 
	       database = result_db_name, 
	       matrix_table = tempResultTable, 
		   matrix_id_value = "",
		   matrix_id_colname = "", 
		   row_id_colname = "OutputRowNum", 
		   col_id_colname = "OutputColNum", 
		   cell_val_colname = "OutputValL",
		   whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutputValL IS NOT NULL "))


	# calculating U FLmatrix
    u<-FLMatrix( 
	       connection = connection, 
	       database = result_db_name, 
	       matrix_table = tempResultTable, 
		   matrix_id_value = "",
		   matrix_id_colname = "", 
		   row_id_colname = "OutputRowNum", 
		   col_id_colname = "OutputColNum", 
		   cell_val_colname = "OutputValU",
		   whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutputValU IS NOT NULL "))

	# calculating perm FLVector
	VID1 <- max_vector_id_value
	max_vector_id_value <<- max_vector_id_value + 1
	table <- FLTable(connection,
		             result_db_name,
		             tempResultTable,
		             "OutputColNum",
		             whereconditions=paste0(getRemoteTableName(result_db_name,tempResultTable),".OutputPermut = 1 ")
		             )

	perm <- table[,"OutputRowNum"]

	sqlstr <- paste(sqlstrLU1,sqlstrLU2)
	
	sqlSendUpdate(connection,sqlstr)

	LUMatrix  <- FLMatrix( 
			       connection = connection, 
			       database = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = MID1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL")

	# calculating x FLVector
	VID2 <- max_vector_id_value
	max_vector_id_value <<- max_vector_id_value + 1

	sqlstrX <-paste0("INSERT INTO ",
						getRemoteTableName(result_db_name,result_vector_table),
						" SELECT ",VID2,
								",ROW_NUMBER() OVER(ORDER BY ",LUMatrix@variables$colId,",",LUMatrix@variables$rowId,")
	                       		, ",LUMatrix@variables$value,"
						  FROM ",remoteTable(LUMatrix),
						 constructWhere(constraintsSQL(LUMatrix)))
	
	sqlSendUpdate(connection,sqlstrX)
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_INDEX",
		             whereconditions=paste0(result_db_name,".",result_vector_table,".VECTOR_ID = ",VID2)
		             )

	x <- table[,"VECTOR_VALUE"]

	# calculating Dim FLVector
	Dim<- dim(data_perm)

	a<-new("FLLU",
		x=x,
		perm=perm,
		Dim=Dim,
		lower=l,
		upper=u,
		data_perm = data_perm
	)
	class(a)<-"FLLU"

	#sqlSendUpdate(connection,paste0(" DROP TABLE ",getRemoteTableName(result_db_name,tempResultTable)))
	return(a)
}

print.FLLU<-function(object){
	note1<-length(object@x)
	note2<-length(object@perm)
	note3<-length(object@Dim)
	cat("'Matrix Factorization' of Formal class 'denseLU' [package Matrix] with 3 slots\n") #"Matrix"
	cat("..@x	: num[1:",note1,"]")
	print(object@x)
	cat("..@perm	: int[1:",note2,"]")
	print(object@perm)
	cat("..@Dim	: int[1:",note3,"]")
	print(object@Dim)
}

setMethod("show","FLLU",print.FLLU)

expand<-function(x, ...){
	UseMethod("expand",x)
}

expand.default <- Matrix::expand

expand.FLLU <- function(object)
{
	return(list(L=object@lower,
				U=object@upper,
				P=object@data_perm))
}


`$.FLLU`<-function(object,property){
	if(property=="L"){
		object@lower
	}
	else if(property=="U"){
		object@upper
	}
	else if(property=="P"){
		object@data_perm
	}
	else "That's not a valid property"
}

