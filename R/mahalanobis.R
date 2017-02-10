#' Mahalanobis Distance
#'
#' Returns the squared Mahalanobis distance of an FLVector x and FLVector y with
#' respect to a covariance matrix sigma = S.
#'                          D^2 = (x - y)' Σ^-1 (x - y)
#' 
#' @param x An FLVector of data with say, p columns.
#' @param y An FLVector of data with say, p columns.
#' @param S A covariance matrix of the distribution(p*p)
#' 
#' @examples
#' x<-FLVector(c(0,0))
#' y<-FLVector(1:2)
#' ma<-cbind(1:6,1:3)
#' s<-var(ma)
#' S<-as.FLMatrix(s)
#' mahalanobis(x,y,S)
#' @export

mahalanobis<-function(x,y,S,...){
	UseMethod("mahalanobis",x)
}

mahalanobis.default<-stats::mahalanobis

mahalanobis.FLVector<-function(x,y,S,...){
    ##browser()
	#if(!class(x)||!class(y)=="FLVector") stop("Function only applies to numeric FLVectors of x and y")
	#if(!class(S)=="FLMatrix") stop("Function works for a covariance matrix")

	t <- constructUnionSQL(pFrom=c(a=constructSelect(x),
								   b=constructSelect(y),
								   c=constructSelect(S)),
                           pSelect=list(a=c(pGroupID=1,
                           					pID=1,
                           					pRowID=1,
                           					pColID="a.vectorIndexColumn",
                           					pValue="CAST(a.vectorValueColumn AS FLOAT)"),
                                        b=c(pGroupID=1,
                                        	pID=2,
                                        	pRowID=1,
                                        	pColID="b.vectorIndexColumn",
                                        	pValue="CAST(b.vectorValueColumn AS FLOAT)"),
                                        c=c(pGroupID=1,
                                        	pID=0,
                                        	pRowID="c.rowIdColumn",
                                        	pColID="c.colIdColumn",
                                        	pValue="CAST(c.valueColumn AS FLOAT)")))

    p <- createTable(pTableName=gen_unique_table_name("temp"),pSelect=t,pTemporary=TRUE)
	query<-constructUDTSQL(pViewColnames=c(pGroupID="pGroupID",pID="pID",pRowID="pRowID",pColID="pColID",pValue="pValue"),
						   pSelect=paste0("Select * from ",p),
						   pOutColnames="a.*",
						   pFuncName="FLMahaDistUdt",
						   pLocalOrderBy=c("pGroupID","pID"))
	tName <- gen_unique_table_name("mahalanobis")
    p <- createTable(tName,pSelect=query,pTemporary=TRUE)
    a<-sqlQuery(getFLConnection(),paste0("Select * from ",p))
    val<-as.numeric(a[2])
	return(val*val)
}
