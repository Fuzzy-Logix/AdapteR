#' @include utilities.R
#' @include FLMatrix.R
NULL
library(Matrix)
#' An S4 class to represent LU Decomposition
#' @slot x object of class numeric
#' @slot perm object of class integer
#' @slot Dim object of class integer
#' @slot lower object of class matrix
#' @slot upper object of class matrix
#' @slot data_perm object of class matrix
setClass(
	"FLLU",
	slots=list(
		x="numeric",
		perm="integer",
		Dim="integer",
		lower="matrix",
		upper="matrix",
		data_perm="matrix"
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
#' LU Decomposition.
#'
#' The LU decomposition involves factorizing a matrix as the product of a lower
#' triangular matrix L and an upper triangular matrix U. Permutation matrix is also provided in the output.
#' If permutation matrix is not used in the decomposition, the output of permutation matrix is an identity matrix.
#'
#' \code{lu} replicates the equivalent lu() generic function.\cr
#' The wrapper overloads lu and implicitly calls FLLUDecompUdt.\cr\cr
#' \code{expand} decomposes the compact form to a list of matrix factors.\cr
#' The expand method returns L,U and P factors as a list of matrices.\cr
#'
#' The decomposition is of the form A = P L U where typically all matrices are of size (n x n),
#' and the matrix P is a permutation matrix, L is lower triangular and U is upper triangular.
#' @method lu FLMatrix
#' @param table an object of class FLMatrix
#' @section Constraints:
#' Input can only be a square matrix (n x n) with maximum dimension limitations
#' of (1000 x 1000).
#' @return
#' \item{x}{the "L" (unit lower triangular) and "U" (upper triangular) factors of the original matrix}
#' \item{perm}{a vector of length min(Dim) that describes the permutation applied to the rows of the original matrix}
#' \item{Dim}{the dimension of the original matrix}
#' \item{lower}{lower triangular matrix}
#' \item{upper}{upper triangular matrix}
#' \item{data_perm}{permutation matrix}
#' @examples
#' connection<-odbcConnect("Gandalf")
#' table <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMulti", 2)
#' lu(table)
#' expand(lu(table))
#' expand(lu(table))$L
#' expand(lu(table))$U
#' expand(lu(table))$P
#' @export
lu.FLMatrix<-function(object){
	connection<-object@odbc_connection
	sqlstr<-paste0("WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS (SELECT a.",object@matrix_id_colname,", a.",object@row_id_colname,", a.",object@col_id_colname,", a.",object@cell_val_colname," FROM  ",object@matrix_table," a WHERE a.",object@matrix_id_colname," = ",object@matrix_id_value,") SELECT a.* FROM TABLE (FLLUDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID, z.Cell_Val) HASH BY z.Matrix_ID LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID) AS a ORDER BY 1,2,3;")
	retobj<-sqlQuery(connection,sqlstr)
	nrow<-max(retobj$OutputRowNum)
	ncol<-max(retobj$OutputColNum)
	cellval_perm<-(retobj$OutputPermut)
	limit<-sqrt(length(cellval_perm))
	data_perm<-matrix(cellval_perm,nrow,ncol,byrow=TRUE)
	position<-which(data_perm!=0,arr.ind=TRUE)
	perm<-position[,1]
	Dim<-dim(data_perm)

	l<-(matrix(retobj$OutputValL,nrow,ncol,byrow=TRUE))
	u<-(matrix(retobj$OutputValU,nrow,ncol,byrow=TRUE))

	x<-c()
	for(p in 1:nrow) {
		for(q in 1:p) {
			if (q > ncol)
				break;
			x<-c(x, u[q,p])
		}
		for(r in p+1:ncol) {
			if (r > ncol)
				break;
			x<-c(x, l[r,p])
		}
	}

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
	cat("..@x	: num[1:",note1,"]",object@x[1:5],"...\n") #"Spacing before .. after @ and no spacing after :"
	cat("..@perm	: int[1:",note2,"]",object@perm,"\n") #Try using trim function
	cat("..@Dim	: int[1:",note3,"]",object@Dim,"\n")
}

setMethod("show","FLLU",print.FLLU)

setGeneric("expand", function(object) {
  standardGeneric("expand")
})

setMethod("expand",signature(object = "FLLU"),definition = function(object){
	a<- new("expandFLLU",
		luobject = object
	)
	a
})

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


