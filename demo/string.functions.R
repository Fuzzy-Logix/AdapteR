## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## This demo shows how the
## AdapteR package of Fuzzy Logix is
## easing interaction with the DB Lytix(TM) in-database
## library.
##
## The demo covers string functions
#############################################################
if(!exists("connection")) {
    demo("connecting", package="AdapteR")
}
options(debugSQL=FALSE)
#############################################################

## ActressLDist table has information about hollywood movies
## Examining table schema
sqlQuery(connection,paste0(
"SELECT TOP 10 * ",
" FROM ActressLDist "))

vtemp <- readline("Table is wide and has obsid as unique index:")

FLTblObj <- FLTable(table="ActressLDist",
                    obs_id_colname="ObsID")

vtemp <- readline("Above: FLTable object created for the table")

## Examining the data using head on FLTable object
head(FLTblObj)

vtemp <- readline("Above: Using head to preview data")

##******* Finding the top 5 Actress' names similar to 'Aleandro Formal' *******
FLVecObj <- FLTblObj[,"Actress"]

vtemp <- readline("Above: Create a FLVector of Actress names by subsetting FLTable")

## Using Levenshtein distance measure to find top similarities
distFLVec <- stringdist("Aleandro Formal",
                        FLVecObj,
                        method="lv",
                        caseFlag=1)
top5Matches <- FLVecObj[head(order(distFLVec),5)]
top5Matches

vtemp <- readline("Above: Top 5 matches are found with no data fetching")

##******* Finding the information about any 'Sherlock Holmes' movies **********
FLVecObj <- FLTblObj[,"FilmTitle"]
matching_index <- grep(pattern="Sherlock Holmes",
                      x=FLVecObj,
                      ignore.case=TRUE)
### Display matching Film names
FLVecObj[matching_index]

### Display Film and Lead Actors who played Sherlock Holmes
FLTblObj[matching_index,c("FilmTitle","Actor")]

vtemp <- readline("Above: Demonstrates pattern matching capabilities of AdapteR")

##**************** Finding if 'StarWars' exists and replacing 'S' with 'A' **********
if(any(grepl("Star Wars",FLVecObj))){
    vresult <- gsub("S","A",FLVecObj)
    print(vresult[grep("Star Wars",FLVecObj)])
}

vtemp<- readline("Above: Demonstrates support of expressions in AdapteR")

###...END...###
###...Thank You...###