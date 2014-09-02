notNaturalNumber <- function(x)
{
	if (class(x) != "numeric") 
        return(TRUE);
		
    if (ceiling(x) != floor(x)) 
        return(TRUE);
		
	if (x<=0) 
        return(TRUE);
		
	return(FALSE);
}

FLDecisionTree <- function(TableName, ObsIDColName, VarIDColName, ValueColName, NumOfSplits, MaxLevel, PurityThreshold, Note)
{

    #check TableName
    if (class(TableName) != "character") 
        return("First parameter is the table name and it should be a string");
	
	#check ObsIDColName
    if (class(ObsIDColName) != "character") 
        return("Second parameter is the name of ObsID column and it should be a string");
		
	#check VarIDColName
    if (class(VarIDColName) != "character") 
        return("Third parameter is the name of the VarID column and it should be a string");
		
	#check ValueColName
    if (class(ValueColName) != "character") 
        return("Fourth parameter is the name of the value column and it should be a string");
		
	#check NumOfSplits
    if (notNaturalNumber(NumOfSplits)) 
        return("Fifth parameter is the maximum number of breakpoints used to split a continuous variable's value and it should be a positive integer");
		
    #check MaxLevel 
    if (notNaturalNumber(MaxLevel)) 
        return("Sixth parameter is the maximum level the tree can grow and it should be a positive integer");
    
    #check PurityThreshold
    if (class(PurityThreshold) != "numeric") 
        return("Seventh parameter is the leaf node purity threshold and it should be numeric");
    
    if (PurityThreshold <= 0.5 || PurityThreshold >= 1) 
        return("Seventh parameter is the leaf node purity threshold and it should lie between 0.5 and 1");

    #check note
    if (class(Note) != "character") 
        return("Eighth parameter is the user defined comment and it should be a string");
	
    #Generate SQL
    SQLStr <- paste("CALL FLDecisionTree('", TableName, "','", ObsIDColName, "','", VarIDColName, "','", ValueColName, "','", NumOfSplits, "','", MaxLevel, "','", PurityThreshold, "','", Note, "','", "AnalysisID)");


