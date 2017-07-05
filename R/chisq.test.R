NULL

#' Chi-square and Pearson Chi-square Test
#'
#' Produces the expected and the chi-square value for each cell in the contingency
#' table.
#'
#' @param x FLMatrix
#' @param pear The values is 1 if wanna perform pearson chi-square test. Otherwise, 
#' by default it's 0 and perform chi- square test.
#' @return A list with class "htest" outputting the corresponding expected and P Values.
#' @examples
#' mat <- rbind(c(762, 327, 468), c(484, 239, 477))
#' dimnames(mat) <- list(gender = c("F", "M"),
#'                    party = c("Democrat","Independent", "Republican"))
#' FLmat <- as.FL(mat)
#' chisq.test(FLmat)  
#'       ## by default pear=0 
#'       ## performs chi-square test
#' chisq.test(FLmat,pear=1) 
#'        ## performs Pearson chi- square test
#' @export
setGeneric("chisq.test",function(x,...)
				standardGeneric("chisq.test"))

setMethod("chisq.test",signature(x="ANY"),
		function(x,...){
			return(stats::chisq.test(x,...))
			})

setMethod("chisq.test",signature(x="FLMatrix"),
	function(x,pear=0,...){
        #browser()
        checkHypoSystemTableExists()
		if(!is.FLMatrix(x)) stop("Only FLMatrix objects are supported")
			if(pear==0){
			ifelse(is.null(rownames(x)),vrownames<-1:nrow(x),vrownames<-rownames(x))
			ifelse(is.null(colnames(x)),vcolnames<-1:ncol(x),vcolnames<-colnames(x))
		
			vrownames<-as.FLVector(vrownames)
			vcolnames<-as.FLVector(vcolnames)
			pFuncName<-"FLChiSq"
            vsqlstr <- constructHypoTestsScalarQuery(pFuncName = pFuncName,
                                                pFuncArgs = c("a.vectorValueColumn",
                                                            "b.vectorValueColumn",
                                                            "c.rowIdColumn",
                                                            "c.colIdColumn",
                                                            "c.valueColumn"),
                                                pStats=c("chi_sq","exp_val"),
                                                pFrom=c(a=constructSelect(vrownames),
                                                        b=constructSelect(vcolnames),
                                                        c=constructSelect(x)),
                                                pGroupBy=c(rowname="a.vectorValueColumn",
                                                           colname="b.vectorValueColumn"),
                                                pOrderBy=c("a.vectorValueColumn",
                                                           "b.vectorValueColumn"))

			# vsqlstr   <-  constructAggregateSQL(pFuncName=pFuncName,
   #          	                            	pFuncArgs=c("f.FLStatistic",
   #              	                            	        "a.vectorValueColumn",
   #                  	                            	    "b.vectorValueColumn",
   #                      	                            	"c.rowIdColumn",
   #                          	                        	"c.colIdColumn",
   #                              	                    	"c.valueColumn"),
   #                                  	    	pAddSelect=c(rowname="a.vectorValueColumn",
   #                                      					 colname="b.vectorValueColumn",
   #                                      					 stat="f.FLStatistic"),
	  #                                       	pFrom=c(a=constructSelect(vrownames),
   #  	                                    			b=constructSelect(vcolnames),
   #      	                                			c=constructSelect(x),
   #          	                                   		f="fzzlARHypTestStatsMap"),
   #              	                        	pWhereConditions="f.FLFuncName='FLChiSq'",
   #                  	                    	pGroupBy=c(rowname="a.vectorValueColumn",
   #                      	                			   colname="b.vectorValueColumn",
   #                          	            			   stat="f.FLStatistic"),
   #                              	        	pOrderBy=c("a.vectorValueColumn","b.vectorValueColumn"))
			vres<-sqlQuery(connection,vsqlstr)
			return(vres)}
		else{
			pFuncName<-"FLPearsonChiSq"
            x <- setAlias(x,"")
            vWhereClause <- setdiff(getWhereConditionsSlot(x),"")
            if(length(vWhereClause)==0)
                vWhereClause <- "NULL"
			pTableName <- getTableNameSlot(x)
			## asana ticket-https://app.asana.com/0/150173007236461/182190129148838
			vres <-	sqlStoredProc(connection,
								    pFuncName,
									InputTable=pTableName,
									RowName="rowIdColumn",
									ColName="colIdColumn",
									CountName="valueColumn",
									WhereClause=vWhereClause,
									GroupBy="MATRIX_ID",
									TableOutput=1,
									outputParameter=c(ResultTable="resTable"))
            colnames(vres) <- tolower(colnames(vres))
            if(!is.null(vres$resulttable)){
                vres <- as.character(vres$resulttable)
                ret <- sqlQuery(connection,
                            paste0("SELECT chisq AS chisq,p_value as p_value \n ",
                                    "FROM ",vres))
            }else ret <- vres
			vres<-list(p.value=ret$p_value[1],
					  statistic=ret$chisq[1])
			class(vres)<-"htest"
			return(vres)
		}	
	}
)
