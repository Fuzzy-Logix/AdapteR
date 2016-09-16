
setGeneric("chisq.test",function(x,...)
				standardGeneric("chisq.test"))
setMethod("chisq.test",signature(x="ANY"),
		function(x,...){
			return(stats::chisq.test(x,...))
			})
setMethod("chisq.test",signature(x="FLMatrix"),
	function(x,pear=0,...){
		if(!is.FLMatrix(x)) stop("Only FLMatrix objects are supported")
			if(pear==0){
			ifelse(is.null(rownames(x)),vrownames<-1:nrow(x),vrownames<-rownames(x))
			ifelse(is.null(colnames(x)),vcolnames<-1:ncol(x),vcolnames<-colnames(x))
		
			vrownames<-as.FLVector(vrownames)
			vcolnames<-as.FLVector(vcolnames)
		
			pFuncName<-"FLChiSq"
			vsqlstr   <-  constructAggregateSQL(pFuncName=pFuncName,
            	                            	pFuncArgs=c("f.FLStatistic",
                	                            	        "a.vectorValueColumn",
                    	                            	    "b.vectorValueColumn",
                        	                            	"c.rowIdColumn",
                            	                        	"c.colIdColumn",
                                	                    	"c.valueColumn"),
                                    	    	pAddSelect=c(rowname="a.vectorValueColumn",
                                        					 colname="b.vectorValueColumn",
                                        					 stat="f.FLStatistic"),
	                                        	pFrom=c(a=constructSelect(vrownames),
    	                                    			b=constructSelect(vcolnames),
        	                                			c=constructSelect(x),
            	                                   		f="fzzlARHypTestStatsMap"),
                	                        	pWhereConditions="f.FLFuncName='FLChiSq'",
                    	                    	pGroupBy=c(rowname="a.vectorValueColumn",
                        	                			   colname="b.vectorValueColumn",
                            	            			   stat="f.FLStatistic"),
                                	        	pOrderBy=c("a.vectorValueColumn","b.vectorValueColumn"))
			vres<-sqlQuery(connection,vsqlstr)
			return(vres)}
		else{
			browser()
			pFuncName<-"FLPearsonChiSq"
			pTableName<-x@select@table_name
			## asana ticket-https://app.asana.com/0/150173007236461/182190129148838
			vres<-	  sqlStoredProc(connection,
								    pFuncName,
									InputTable=pTableName,
									RowName="rowIdColumn",
									ColName="colIdColumn",
									CountName="valueColumn",
									WhereClause=NULL,
									GroupBy="MATRIX_ID",
									TableOutput=1,
									outputParameter=c(ResultTable="resTable"))
			return(resTable)
		}	
	}
)