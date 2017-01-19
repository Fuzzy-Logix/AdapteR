##methods for increase in speed:
##dbBulkCopy
## sqlSave function in RODBC
## save file on local folder and update.


##links:
##http://stackoverflow.com/questions/19190744/how-to-quickly-export-data-from-r-to-sql-server?noredirect=1&lq=1
##
##https://developer.teradata.com/connectivity/articles/speed-up-your-jdbcodbc-applications
## Benchmarking for RODBC , RJDBC, rsqlserver
##https://github.com/agstudy/rsqlserver/wiki/benchmarking

library(RODBCDBI)
library(RODBC)
library(DBI)

## RODBCDBI base package connection.(page 10 RODBCDBI)
rodbi <- dbConnect(RODBCDBI::ODBC(), dsn = "Gandalf", user = "AReddy", password = "fzzlpass")

## RODBCDBI connection through flconnect.
connection <- flConnect(odbcSource = "Gandalf", database = "FL_DEMO", platform = "TD")

##flconnect:jdbc
connection <- flConnect("jdbc:teradata://10.200.4.116",
                      "FL_DEMO",
                      "mbondre","fzzlpass",
                      c("F:/terajdbc4.jar",
                         "F:/tdgssconfig.jar"),
                      debug=FALSE)


# RODBC base package connection.
rodb <-odbcConnect("Gandalf", uid = "AReddy", pwd = "fzzlpass")

##FL Connection:
connection <- flConnect(odbcSource = "Gandalf",database = "FL_DEMO",platform = "TD")

##  Gandalf was really slow so cant be judged on this.
##  RODBCDBI
##  user    system elapsed 
##  13.25    2.59  691.55 
##  LinRegr Table
query <- paste0("SELECT * FROM tbllinRegr")
ARtym <- system.time(sqlQuery(connection, query))
rodbtym <- system.time(sqlQuery(rodb, query))
rodbitym <- system.time(dbGetQuery(rodbi, query))


## LDA time difference.
system.time(t <- dbGetQuery(rodbi, "SELECT * FROM tbllda"))
system.time(w <- sqlQuery(rodb, "SELECT * FROM tbllda"))

# StoredProc:
query <- paste0("CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val',NULL, 2, 20, 2,
'KMeans, clusters=2, maxiter=20, hypothesis=2', AnalysisID );")

rodbitym <- NULL
niter <- 5
lapply(1:niter, function(x){
    assign(rodbitym[[x]],dbGetQuery(rodbi, query),envir = .GlobalEnv )
    })


##individual time

ARtym <- system.time(sqlQuery(connection, query))
rodbtym <- system.time(sqlQuery(rodb, query))
rodbitym <- system.time(dbGetQuery(rodbi, query))

query <- paste0("SELECT * FROM fzzlKMeansInfo WHERE AnalysisID = 'A646958'")
inform <- dbGetQuery(rodbi, query )
#### UDT's Working


res <- dbSendQuery(rodbi, "SELECT * FROM tbllda")
dbGetRowCount(res)
