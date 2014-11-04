# /**
#  * Converts List to where clause
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx='x' AND Vary='y'"
#  */
list.to.whereClause <- function (x) {
	     whereClause <- paste(names(x),x,sep="=\'",collapse="\' AND ");
	     whereClause <- paste(whereClause,"\'",sep="");
	     whereClause <- ifelse(nchar(whereClause) > 1, whereClause, "1=1");
	     whereClause
     }

# /**
#  * Converts List to class Spec. Used for Data Prep
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx(x), Vary(y)"
#  */
list.to.classSpec <- function (x) {
	classSpec <- paste(names(x),x,sep="(",collapse="), ")
	classSpec <- paste(classSpec,")",sep="")
	classSpec <- ifelse(nchar(classSpec) > 1, classSpec, "");
	classSpec
}

# /**
#  * Converts List to class Spec. Used for Data Prep
#  * @param  {list} x e.g. list(Varx="a",Vary="b")
#  * @return {string}   "Varx(x), Vary(y)"
#  */
list.to.excludeClause <- function (x) {
	excludeClause <- paste(x, collapse=", ")
	excludeClause
}

#/**
# * Generates Name for a Deep Table
# * @param  {string} TableName Name of Wide Table
# * @return {string}           [description]
# */
GenDeepTableName <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Deep",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

#/**
# * Generates Name for a Include/Exclude Spec
# * @param  {string} TableName Name of Table
# * @return {string}           [description]
# */
GenSpecID <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"spec",round(random_no*random_no*10000),sep="_");
}

#/**
# * Assigns CatToDummy  = 1 if categorical variable(s) is(are) present in the FLTable object
# * @param  {string} ClassSpec list
# * @return {int} Value of CatToDummy
# */
CalcCatToDummy <- function(ClassSpec) {
	if (length(ClassSpec) == 0) 
    CatToDummy <- 0
	else 
    CatToDummy <- 1;
	CatToDummy
}

#/**
# * Generates query to map variable names to variable IDs
# * @param  {string} AnalysisID Wide to Deep Analysis ID
# * @param  {string} Vars Vector
# * @return {string} Query to get mapping of Variable to VariableID
# */
VarNameToID <- function(AnalysisID,Vars){
	
	query <- "SELECT a.COLUMN_NAME,a.Final_VarID as VarID
	FROM fzzlRegrDataPrepMap a 
	WHERE a.AnalysisID = '%s' AND a.COLUMN_NAME IN ('%s')";
	query <- sprintf(query,AnalysisID,paste(Vars,collapse="','",sep=""))
	query <- gsub("[\r\n]", "", query);
	query	
}

#/**
# * Generates Name for an Output Predictions Table
# * @param  {string} TableName Name of Table
# * @return {string}           [description]
# */
GenOutTable <- function(Type,AnalysisID){
	random_no <- rnorm(1);
	paste(Type,"Prediction",AnalysisID,round(random_no*random_no*10000),sep="_");
}

#/**
# * Generates Name for the Output Table for Udt Functions
# * @param {string} MatrixTableName
# * @param {numeric} MatrixID
# * @return {string} 
# */
GenOutMatrixTable <- function(Operation,MatrixTable, Matrix_ID) {
	random_no <- rnorm(1);
	paste(Operation,MatrixTable, Matrix_ID, round(random_no*random_no*10000), sep = "_");
}
		