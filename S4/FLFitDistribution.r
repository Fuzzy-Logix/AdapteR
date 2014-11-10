#
# Fuzzy Logix Fit Distribution Objects

# FLFitContDistr Object
setOldClass("RODBC") 
setClass("FLFitContDistr", 
		slots = list(	ODBCConnection = "RODBC", 
						DBName         = "character",
						TableName      = "character",
						NumValColName  = "character"))
						
# FLFitContDistr Object
setClass("FLFitDiscDistr",
		slots = list(	ODBCConnection = "RODBC", 
						DBName         = "character",
						TableName      = "character",
						NumOfSuccessColName = "character",
						NumOfTrialsColName = "character"))
						
# Constructor function for FLFitDistrObject
FLFitDistrObject <- function(Connection, DBName, TableName, DistributionType, NumValColName = "NumVal", NumOfSuccessColName = "NumOfSuccess", NumOfTrialsColName = "NumOfTrials") {
	# DistributionTypes
	# Cont - Continuous
	# Disc - Discrete
	types <- c("Cont", "Disc")
		if(DistributionType %in% types)
		{
			#if (!is.character(Connection)) 		
			#stop("ConnectionName must be a string")
			if (!is.character(DBName)) 		
			stop("DBName must be a string")
			if (!is.character(TableName))	
			stop("TableName must be a string")
			
			sqlQuery(Connection, paste("DATABASE", DBName));
			sqlQuery(Connection, "SET ROLE ALL");
			
			if(DistributionType == "Cont")
			{
				new("FLFitContDistr", ODBCConnection = Connection, DBName = DBName, TableName = TableName, NumValColName = NumValColName)
			}
			else
			{
				new("FLFitDiscDistr", ODBCConnection = Connection, DBName = DBName, TableName = TableName, NumOfSuccessColName = NumOfSuccessColName, NumOfTrialsColName = NumOfTrialsColName)
			}
		}
		else
		{
			stop("Incorrect value for DistributionType parameter. DistributionType must be in {\"Cont\",\"Disc\"} ")
		}
}

# Wrapper for fitting distribution
FLFitDistr <- function(x, Distribution, Method = "MLE") {
	# Distribution
	# -> Normal
	# -> Weibull
	# -> Binomial
	
	# Method
	# MLE - Maximum Likelihood Estimation
	# MDE - Minimum Distance Estimation
	
	DistributionName <- c("Normal", "Weibull", "Binomial")
	MethodType       <- c("MLE", "MDE")
		if(Distribution %in% DistributionName && Method %in% MethodType) {
			#ViewName <- GenViewName(Distribution, x@TableName)
			if(class(x) == "FLFitContDistr") {
				path <- "SQL//FLFitContDistr.sql";
				stopifnot(file.exists(path));
				sql  <- readChar(path, nchar = file.info(path)$size);
				sql  <- sprintf(	sql, 
							x@NumValColName,
							x@TableName, 
							Method,
							Distribution);
				sql  <- gsub("[\r\n]", "", sql);
				print(sql)
				res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);		
			}
			
			if(class(x) == "FLFitDiscDistr") {
				path <- "SQL//FLFitDiscDistr.sql";
				stopifnot(file.exists(path));
				sql  <- readChar(path, nchar = file.info(path)$size);
				sql  <- sprintf(	sql, 
							x@NumOfTrialsColName,
							x@NumOfSuccessColName,
							x@TableName, 
							Method,
							Distribution);
				sql  <- gsub("[\r\n]", "", sql);
				print(sql)
				res <- sqlQuery(x@ODBCConnection, sql, stringsAsFactors = FALSE);		
			}
			return(res)
		}
		
		else if((Distribution %in% DistributionName) < 1) {
			stop("Incorrect value for Distribution parameter. Distribution must be in {\"Normal\",\"Weibull\",\"Binomial\"} ")
		}
		
		else
		{
			stop("Incorrect value for Method parameter. Method must be in {\"MLE\",\"MDE\"} ")
		}		
}

