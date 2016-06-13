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

readline("First connect to the database:")

if(!exists("yourODBCSource") & !exists("yourUser"))
    stop("Please set the variable \nyourODBCSource <- \"...\" for odbc login!\nor set for jdbc login:\nyourUser <- \"...\"\nyourPassword <- \"...\"")

if(!exists("connection") & exists("yourODBCSource")){
    connection <- flConnect(odbcSource = yourODBCSource)
}

## If ODBC has failed we try to create a JDBC connection
if(!exists("connection")){
    if(!exists("yourUser")) stop("Please set the variable \nyourUser <- \"...\" for jdbc login!")
    if(!exists("yourPassword")) stop("Please set the variable \nyourPassword <- \"...\" for jdbc login!")
    if(!exists("yourJarDir")) yourJarDir <- NULL
    connection <-
        flConnect(
            host     = "10.200.4.116",
            database = "Fl_demo",
            user = yourUser,
            passwd = yourPassword,
            ## set dir.jdbcjars to add jdbc driver
            ## and security jars to classpath:
            ##    terajdbc4.jar tdgssconfig.jar
            ## CAVE: fully qualified PATH required
            dir.jdbcjars = yourJarDir)
    if(!exists("connection")) stop("Please check your username and password\nand possibly set the variable \nyourPassword <- \"...\" for jdbc login!")
}

options(debugSQL=TRUE)
