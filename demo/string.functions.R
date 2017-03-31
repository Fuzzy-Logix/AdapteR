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
#############################################################

## ActressLDist table has information about hollywood movies
## Examining table schema
sqlQuery(connection,limitRowsSQL(paste0(
"SELECT * ",
" FROM ",getTestTableName("ActressLDist")),10))

vtemp <- readline("Table is wide and has obsid as unique index:")

FLTblObj <- FLTable(table=getTestTableName("ActressLDist"),
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


## in-database sorting
O <- order(distFLVec)

selection <- head(O,5)

data.frame(Actress  = as.R(FLVecObj[selection]),
           distance = as.R(distFLVec[selection]))

vtemp <- readline("Above: Top 5 matches are found.  Data fetched only for printed 5 matches")

distFL <- stringdistmatrix(FLVecObj,c("Aleandro Formal","John Wayn"),method="dl")

head(distFL)

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

run.FLStringDistShiny <- function (){
    require(plyr)
    ## #########################################################
    ## Shiny web application Demo
    ##
    ## A search string can be inserted
    ## and results are shown after matching
    FLTblObj <- FLTable(table=getTestTableName("ActressLDist"),
                        obs_id_colname="ObsID",
                        whereconditions = "Actor<>'' AND Actress<>'' AND Director<>'' AND FilmTitle<>''")
    findName <- function(FLTblObj,
                         text,
                         columnName="Actor",
                         method="lv",
                         N=10){
        ## Using Levenshtein distance measure to find top similarities
        distFLVec <- stringdist(text,
                                FLTblObj[[columnName]],
                                method=method,
                                caseFlag=1)
        ## in-database sorting
        O <- order(distFLVec)
        selection <- head(O,N)
        return(cbind(
            dist=as.vector(distFLVec[selection]),
            as.data.frame(FLTblObj[selection,])))
    }
    ##findName(FLTblObj,"Jim","Director")
    require(R.utils)
    if (!requireNamespace("shiny", quietly = TRUE)){
        install.packages("shiny")
    }
    require(shiny)
    shinyApp(
        ui = fluidPage(
            fluidRow(
                column(3,
                       textInput("freetext", "Name:","Eastwoard, Clint")),
                column(3,
                       selectInput(
                           "columns", "Columns:",
                           choices = c("Actor","Actress","Director","Filmtitle"),
                           selected = "Actor",
                           multiple = FALSE)),
                column(3,
                       selectInput(
                           "method", "Method:",
                           choices = c(Levenshtein="lv", "Levenshtein-Damerau"="dl",
                                       "Jaro-Winkler"="jw"), ##, "Needleman-Wunsch"="nmw"
                           selected = "lv",
                           multiple = FALSE))),
            fluidRow(tableOutput("matches"))
        ),
        server = function(input, output) {
        Matches <- reactive({
            text <- input$freetext
            method <- input$method
            a <- ldply(input$columns,
                       function(colName){
                rr <- data.frame()
                tryCatch({
                    rr <- findName(FLTblObj,text,colName,method=method)
                    rr[,toupper(colName)] <- paste0("<b>",rr[,toupper(colName)],"</b>")
                    rr},error=function(e) print(e))
                rr$OBSID <- rr$IMAGE <- NULL
                print(rr)
                rr
            })
            a <- a[order(a$dist),]
            if(is.null(a)) return(data.frame())
            a$Image <- NULL
            a$vectorIndexColumn <- NULL
            a
        })
        output$matches <- renderTable(Matches(),
                                      include.rownames=FALSE,
                                      sanitize.text.function = function(x) x)
    }
    )
}

vtemp <- readline("To explore string matches interactively, we defined a function above. \n Simply execute\n> run.FLStringDistShiny()\nafter ending the Demo now:")


###...END...###
###...Thank You...###
