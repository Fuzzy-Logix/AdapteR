## Fuzzy Logix DB Lytix(TM)
## is a high-speed in-database analytics library
## written in C++, exposing ~700 functions through SQL.
## SQL as low-level language makes analyses
## consumable from all SQL-enabled clients.

## This demo shows how the
## AdapteR package of Fuzzy Logix is
## connecting with the DB Lytix(TM) in-database
## library.

## Setting up a connection can be either done with
## ODBC or JDBC
## Setting up a ODBC connection

## The Documentation for flConnect can be found using ?flConnect
## ?flConnect
vtemp <- readline("This demo shows how to connect to the FL_TRAIN database.\nIf the demo fails, please set the variables mentioned in the error message which are required for connecting with this demo.\n\nPress <ENTER> to connect to your database.")

if(!exists("yourPlatform"))
    yourPlatform <- "TD"

if(!exists("yourODBCSource") & !exists("yourUser"))
    stop("Please set the variable \nyourODBCSource <- \"...\" for odbc login!\nor set for jdbc login:\nyourUser <- \"...\"\nyourPassword <- \"...\"")

## If the fuzzylogix installed database is different from FL_TRAIN(TD),fuzzylogix(TDAster),dblytix(Hadoop)
## Set it using "yourFLDBName" variable before starting this demo.
if(!exists("yourFLDBName")){
    vmap <- c(TD="FL_TRAIN",TDAster="fuzzylogix",Hadoop="dblytix")
    yourFLDBName <- vmap[yourPlatform]
}

?flConnect
## If the Data database is different from FL_TRAIN(TD),fuzzylogix(TDAster),mazdoo(Hadoop)
## Set it using "yourDataDBName" variable before starting this demo.
if(!exists("yourDataDBName"))
    yourDataDBName <- NULL
if(!exists("connection") & exists("yourODBCSource")){
    if(!exists("yourPlatform"))
    stop("Please set the variable \nyourPlatform <- \"...\" for odbc login!\n")
    
    ### ................###########..........................
    ###..........flConnect called for ODBC connection........
    ### ................##########................
    
    connection <- flConnect(odbcSource = yourODBCSource,
                            database=yourFLDBName,
                            platform=yourPlatform,
                            TestDatabase=yourDataDBName)
}

## If ODBC has failed we try to create a JDBC connection
if(!exists("connection")){
    if(!exists("yourUser")) stop("Please set the variable \nyourUser <- \"...\" for jdbc login!")
    if(!exists("yourPassword")) stop("Please set the variable \nyourPassword <- \"...\" for jdbc login!")
    if(!exists("yourJarDir")) yourJarDir <- NULL
    if(!exists("yourHost")) stop("Please set the variable \nyourHost <- \"...\" for jdbc login!")
    
    ### ................###########..........................
    ###..........flConnect called for JDBC connection........
    ### ................##########................

    connection <- flConnect(
                    host     = yourHost,
                    database = yourFLDBName,
                    user = yourUser,
                    passwd = yourPassword,
                    ## set jdbc.jarsDir to add jdbc driver
                    ## and security jars to classpath:
                    ##    terajdbc4.jar tdgssconfig.jar
                    ## CAVE: fully qualified PATH required
                    jdbc.jarsDir = yourJarDir,
                    TestDatabase=yourDataDBName)
    if(!exists("connection")) 
        stop("Please check your username and password\nand possibly set the variable \nyourPassword <- \"...\" for jdbc login!")
}


## .......................#################..................
## ....Trying to Fetch a matrix from FL_TRAIN.tblmatrixmulti using sqlQuery......
## .......................#################..................

sqlQuery(connection,paste0("SELECT * \n FROM ",getTestTableName("tblmatrixmulti")," a \n ",
                            "WHERE a.Matrix_ID=1 \n ",
                            "ORDER BY 1,2,3"))

## You are connected if the query above has resulted in a table output
## Thank You
