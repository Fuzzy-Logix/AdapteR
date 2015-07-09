gen_deep_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Deep",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

gen_score_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Score",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}

gen_wide_table_name <- function(TableName){
	random_no <- rnorm(1);
	paste(TableName,"Wide",round(as.numeric(Sys.time())),round(random_no*random_no*10000000),sep="_");
}