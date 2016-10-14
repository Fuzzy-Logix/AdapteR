df <- data.frame(a=as.integer(101),b=1,c=2,d=rnorm(50))


## Demo sqlExecute(parameterized query) takes longer than bulk query

##ODBC TD Connection
if(!is.TD())
connection <- flConnect(odbcSource = "Gandalf",database = "FL_DEMO",platform="TD")
#### Parameterized SQL
vsqlstr <- "INSERT INTO tblmatrixmulti(matrix_id,row_id,col_id,cell_val) values(?,?,?,?)"
odbcTDPsql <- system.time(ps <- sqlExecute(getRConnection(connection),vsqlstr,df))["elapsed"]## ps==1 for success
#### Bulk SQL
vsqlstr <- "INSERT INTO tblmatrixmulti(matrix_id,row_id,col_id,cell_val) values "
vsqlstr <- paste0(apply(df,1,function(x)
                paste0(vsqlstr,"(",paste0(x,collapse=","),")")),collapse = ";")
cat("size(in B) of sql query sent here: ",object.size(vsqlstr)," \n ")
odbcTDBsql <- system.time(ps <- sqlSendUpdate(getRConnection(connection),vsqlstr))["elapsed"]
cat("bulk query faster than parameterized on ODBC TD by: ",odbcTDPsql-odbcTDBsql," sec \n ")
sqlSendUpdate(connection,"DELETE FROM tblmatrixmulti WHERE matrix_id=101")


##ODBC TDAs Connection
if(!is.TDAster())
connection <- flConnect(odbcSource = "AsterVM",database = "fuzzylogix",platform="TDAster")
#### Parameterized SQL
vsqlstr <- "INSERT INTO tblmatrixmulti(matrix_id,row_id,col_id,cell_val) values(?,?,?,?)"
odbcTDAsPsql <- system.time(ps <- sqlExecute(getRConnection(connection),vsqlstr,df))["elapsed"] ## ps==1 for success
#### Bulk SQL
vsqlstr <- "INSERT INTO tblmatrixmulti(matrix_id,row_id,col_id,cell_val) values "
vappend <- paste0(apply(df,1,function(x)
                          paste0("(",paste0(x,collapse=","),")")),collapse = ",")
vsqlstr <- paste0(vsqlstr,vappend)
cat("size(in B) of sql query sent here: ",object.size(vsqlstr)," \n ")
odbcTDAsBsql <- system.time(ps <- sqlSendUpdate(getRConnection(connection),vsqlstr))["elapsed"]
cat("bulk query faster than parameterized on ODBC TD by: ",odbcTDAsPsql-odbcTDAsBsql," sec \n ")
sqlSendUpdate(connection,"DELETE FROM tblmatrixmulti WHERE matrix_id=101")
