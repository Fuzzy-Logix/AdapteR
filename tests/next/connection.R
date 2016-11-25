library(RODBCDBI)
library(RODBC)
library(DBI)


## RODBCDBI base package connection.
rodbi <- dbConnect(RODBCDBI::ODBC(), dsn = "Gandalf", user = "AReddy", password = "fzzlpass")
# RODBC base package connection.
rodb <-odbcConnect("Gandalf", uid = "AReddy", pwd = "fzzlpass")

##FL Connection:
connection <- flConnect(odbcSource = "Gandalf",database = "FL_DEMO",platform = "TD")


## LinRegr Table
query <- paste0("SELECT * FROM tbllinRegr")
ARtym <- system.time(sqlQuery(connection, query))
rodbtym <- system.time(sqlQuery(rodb, query))
rodbitym <- system.time(dbGetQuery(rodbi, query))



# StoredProc:
query <- paste0("CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val',NULL, 2, 20, 2,
'KMeans, clusters=2, maxiter=20, hypothesis=2', AnalysisID );")

ARtym <- system.time(sqlQuery(connection, query))
rodbtym <- system.time(sqlQuery(rodb, query))
rodbitym <- system.time(dbGetQuery(rodbi, query))




query <- paste0("SELECT * FROM fzzlKMeansInfo WHERE AnalysisID = 'A646958'")
inform <- dbGetQuery(rodbi, query )

