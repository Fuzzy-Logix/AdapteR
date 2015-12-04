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
		Dim="FLVector",
		lower="FLMatrix",
		upper="FLMatrix",
		data_perm="FLMatrix"
	)
)



#' An S4 class to represent L,U and P factors as a list of matrices
#' @slot luobject object of class FLLU
setClass(
	"expandFLLU",
	slots=list(
		luobject="FLLU"
	)
)


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
	connection<-object@odbc_connection
	flag3Check(connection)
	flag1Check(connection)
	
	# calculating LU matrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z(Matrix_ID, Row_ID, Col_ID, Cell_Val) 
		           AS (SELECT a.",object@matrix_id_colname,", 
		           	          a.",object@row_id_colname,", 
		           	          a.",object@col_id_colname,", 
		           	          a.",object@cell_val_colname,
		           	   " FROM  ",remoteTable(object)," a 
		           	   WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
	               SELECT ",max_matrix_id_value,
	                      ",a.OutputRowNum
	                       ,a.OutputColNum
	                       , CAST(a.OutputValL AS NUMBER) 
	               FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	               HASH BY z.Matrix_ID 
	               LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
				   WHERE a.OutputRowNum > a.OutputColNum 
				   AND a.OutputValL IS NOT NULL;")

	sqlSendUpdate(connection,sqlstr)

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z(Matrix_ID, Row_ID, Col_ID, Cell_Val) 
		           AS (SELECT a.",object@matrix_id_colname,", 
		           	          a.",object@row_id_colname,", 
		           	          a.",object@col_id_colname,", 
		           	          a.",object@cell_val_colname,
		           	   " FROM  ",remoteTable(object)," a 
		           	   WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
	               SELECT ",max_matrix_id_value,
	                      ",a.OutputRowNum
	                       ,a.OutputColNum
	                       , CAST(a.OutputValU AS NUMBER) 
	               FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	               HASH BY z.Matrix_ID 
	               LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
				   WHERE a.OutputRowNum <= a.OutputColNum 
				   AND a.OutputValU IS NOT NULL;")

	sqlSendUpdate(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	LUMatrix  <- FLMatrix( 
			       connection = connection, 
			       database = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = nrow(object), 
				   ncol = ncol(object), 
				   dimnames = list(c(),c()))

	# calculating x FLVector

	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table, 
	               " SELECT ",max_vector_id_value,
	                        ",ROW_NUMBER() OVER(ORDER BY a.",LUMatrix@col_id_colname,",a.",LUMatrix@row_id_colname,")
	                       , CAST(a.",LUMatrix@cell_val_colname," AS NUMBER)  
	               FROM  ",remoteTable(LUMatrix)," AS a 
				   WHERE a.",LUMatrix@matrix_id_colname,"=",LUMatrix@matrix_id_value)

	sqlSendUpdate(connection,sqlstr)
	

	max_vector_id_value <<- max_vector_id_value + 1

	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	x <- new("FLVector", 
		     table = table, 
		     col_name = table@cell_val_colname, 
		     vector_id_value = max_vector_id_value-1, 
		     size = ((nrow(object))*(ncol(object))))


	# calculating Permutation FLMatrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
		           AS (SELECT a.",object@matrix_id_colname,", 
		           	          a.",object@row_id_colname,", 
		           	          a.",object@col_id_colname,", 
		           	          a.",object@cell_val_colname,
		           	   " FROM  ",remoteTable(object)," a 
		           	   WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
	               SELECT ",max_matrix_id_value,
	                      ",a.OutputRowNum
	                       ,a.OutputColNum
	                       ,a.OutputPermut 
	               FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	               HASH BY z.Matrix_ID 
	               LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
				   WHERE a.OutputPermut IS NOT NULL;")

	sqlSendUpdate(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	data_perm <- FLMatrix( 
			       connection = connection, 
			       database = result_db_name, 
			       matrix_table = result_matrix_table, 
				   matrix_id_value = max_matrix_id_value-1,
				   matrix_id_colname = "MATRIX_ID", 
				   row_id_colname = "ROW_ID", 
				   col_id_colname = "COL_ID", 
				   cell_val_colname = "CELL_VAL",
				   nrow = nrow(object), 
				   ncol = nrow(object), 
				   dimnames = list(c(),c()))


	# finding limit
	limit<-sqrt((nrow(object))*(ncol(object)))

	# calculating perm FLVector
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_vector_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
		           AS (SELECT a.",object@matrix_id_colname,", 
		           	          a.",object@row_id_colname,", 
		           	          a.",object@col_id_colname,", 
		           	          a.",object@cell_val_colname,
		           	   " FROM  ",remoteTable(object)," a 
		           	   WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
	               SELECT ",max_vector_id_value,
	                      ",a.OutputColNum
	                       ,a.OutputRowNum  
	               FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	               HASH BY z.Matrix_ID 
	               LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
				   WHERE a.OutputPermut = 1;")

	sqlSendUpdate(connection,sqlstr)

	max_vector_id_value <<- max_vector_id_value + 1
	
	table <- FLTable(connection,
		             result_db_name,
		             result_vector_table,
		             "VECTOR_ID",
		             "VECTOR_INDEX",
		             "VECTOR_VALUE")

	perm <- new("FLVector", 
		         table = table, 
		         col_name = table@cell_val_colname, 
		         vector_id_value = max_vector_id_value-1, 
		         size = ncol(object))   ###############################  check this for non-square matrices

	#position<-which(data_perm!=0,arr.ind=TRUE)
	#erm<-position[,1]

	# calculating Dim FLVector
	Dim<-as.FLVector(dim(data_perm),object@odbc_connection)

	# calculating l FLmatrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
		           AS (SELECT a.",object@matrix_id_colname,", 
		           	          a.",object@row_id_colname,", 
		           	          a.",object@col_id_colname,", 
		           	          a.",object@cell_val_colname,
		           	   " FROM  ",remoteTable(object)," a 
		           	   WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
	               SELECT ",max_matrix_id_value,
	                      ",a.OutputRowNum
	                       ,a.OutputColNum
	                       ,a.OutputValL  
	               FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	               HASH BY z.Matrix_ID 
	               LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
				   WHERE a.OutputValL IS NOT NULL;")

	sqlSendUpdate(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	nr <- nrow(object)
	nc <- ncol(object)
	if(nrow(object) < ncol(object))
	{
		nc <- nrow(object)
	}

	l<-FLMatrix( 
	       connection = connection, 
	       database = result_db_name, 
	       matrix_table = result_matrix_table, 
		   matrix_id_value = max_matrix_id_value-1,
		   matrix_id_colname = "MATRIX_ID", 
		   row_id_colname = "ROW_ID", 
		   col_id_colname = "COL_ID", 
		   cell_val_colname = "CELL_VAL",
		   nrow = nr, 
		   ncol = nc, 
		   dimnames = list(c(),c()))


	# calculating U FLmatrix
	sqlstr<-paste0("INSERT INTO ",result_db_name,".",result_matrix_table,
				   " WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) 
		           AS (SELECT a.",object@matrix_id_colname,", 
		           	          a.",object@row_id_colname,", 
		           	          a.",object@col_id_colname,", 
		           	          a.",object@cell_val_colname,
		           	   " FROM  ",remoteTable(object)," a 
		           	   WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") 
	               SELECT ",max_matrix_id_value,
	                      ",a.OutputRowNum
	                       ,a.OutputColNum
	                       ,a.OutputValU   
	               FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) 
	               HASH BY z.Matrix_ID 
	               LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a 
				   WHERE a.OutputValU IS NOT NULL;")

	sqlSendUpdate(connection,sqlstr)

	max_matrix_id_value <<- max_matrix_id_value + 1

	nr <- nrow(object)
	nc <- ncol(object)
	if(nrow(object) > ncol(object))
	{
		nr <- ncol(object)
	}

	u<-FLMatrix( 
	       connection = connection, 
	       database = result_db_name, 
	       matrix_table = result_matrix_table, 
		   matrix_id_value = max_matrix_id_value-1,
		   matrix_id_colname = "MATRIX_ID", 
		   row_id_colname = "ROW_ID", 
		   col_id_colname = "COL_ID", 
		   cell_val_colname = "CELL_VAL",
		   nrow = nr, 
		   ncol = nc, 
		   dimnames = list(c(),c()))


	a<-new("FLLU",
		x=x,
		perm=perm,
		Dim=Dim,
		lower=l,
		upper=u,
		data_perm = data_perm
	)
	class(a)<-"FLLU"
	a
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
	a<- new("expandFLLU",
		luobject = object
	)
	a
}
print.expandFLLU<-function(object){
	cat("$L\n")
	print(object@luobject@lower)
	cat("$U\n")
	print(object@luobject@upper)
	cat("$P\n")
	print(object@luobject@data_perm)
}

setMethod("show","expandFLLU",print.expandFLLU)

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

`$.expandFLLU`<-function(object,property){
	if(property=="L"){
		object@luobject@lower
	}
	else if(property=="U"){
		object@luobject@upper
	}
	else if(property=="P"){
		object@luobject@data_perm
	}
	else "That's not a valid property"
}


