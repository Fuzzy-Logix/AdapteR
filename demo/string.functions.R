## Demo in-DB string functions AdapteR ##
options(debugSQL=FALSE)
if(!exists("connection")) {
  stop("Please run demo(connecting) to create connection object \n")
}
#############################################################

FLTblObj <- FLTable(table=getTestTableName("ActressLDist"),
                    obs_id_colname="obsid")

vtemp <- readline("Above: FLTable object created for the table \n ")

head(FLTblObj)

vtemp <- readline("Above: Using head to preview data \n ")

##******* Finding the top 5 Actress' names similar to 'Aleandro Formal' *******
FLVecObj <- FLTblObj[,"actress"]

vtemp <- readline("Above: Create a FLVector of Actress names by subsetting FLTable \n ")

## Using Levenshtein distance measure to find top similarities
distFLVec <- stringdist("Aleandro Formal",
                        FLVecObj,
                        method="lv",
                        caseFlag=1)

O <- order(distFLVec)

selection <- head(O,5)

data.frame(Actress  = as.R(FLVecObj[selection]),
           distance = as.R(distFLVec[selection]))

vtemp <- readline("Above: Top 5 matches are found.  Data fetched only for printed 5 matches \n ")

distFL <- stringdistmatrix(FLVecObj,c("Aleandro Formal","John Wayn"),method="dl")
head(distFL)
vtemp <- readline("Above: Find distances to Aleandro Formal,John Wayn \n ")

##******* Finding the information about any 'Sherlock Holmes' movies **********
FLVecObj <- FLTblObj[,"filmtitle"]
matching_index <- grep(pattern="Sherlock Holmes",
                       x=FLVecObj,
                       ignore.case=TRUE)

### Display matching Film names
FLVecObj[matching_index]

### Display Film and Lead Actors who played Sherlock Holmes
FLTblObj[matching_index,c("filmtitle","actor")]

vtemp <- readline("Above: Demonstrates pattern matching capabilities of AdapteR")

##**************** Finding if 'StarWars' exists and replacing 'S' with 'A' **********
if(any(grepl("Star Wars",FLVecObj))){
    vresult <- gsub("S","A",FLVecObj)
    print(vresult[grep("Star Wars",FLVecObj)])
}

vtemp<- readline("Above: Demonstrates support of expressions in AdapteR")

###...END...###
###...Thank You...###
