## Demo Connecting to Database ##
vtemp <- readline("This demo shows how to connect to the database.\nIf the demo fails, please set the variables mentioned in the error message which are required for connecting with this demo.\n\nPress <ENTER> to connect to your database.")

if(!exists("connection") & exists("yourODBCSource")){
    connection <- flConnect(odbcSource = yourODBCSource,
                            database=yourFLDBName,
                            platform=yourPlatform,
                            TestDatabase=yourDataDBName,
                            drop=FALSE)
}
else{
    connection <- flConnect(
                    host     = yourHost,
                    database = yourFLDBName,
                    user = yourUser,
                    passwd = yourPassword,
                    jdbc.jarsDir = yourJarDir,
                    TestDatabase=yourDataDBName,
                    drop=FALSE)
}

## check connection works
sqlQuery(connection,paste0("SELECT * \n FROM ",
                            getTestTableName("tblmatrixmulti"),
                            " a \n ",
                            "WHERE a.Matrix_ID=1 \n ",
                            "ORDER BY 1,2,3"))
