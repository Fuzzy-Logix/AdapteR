## you will need variables host,user,passwd,database defined prior to running this code.
##
## equivalent to
## connection <- odbcConnect(ODBCsource)
cat(paste0("Connecting to ",user,"@",host,"/",database," ... "))
library(RJDBC) 

myConnect <- function(){
    ## add jdbc driver and security jars to classpath
    .jaddClassPath("/Users/gregor/fuzzylogix/terajdbc4.jar")
    .jaddClassPath("/Users/gregor/fuzzylogix/tdgssconfig.jar")
    library(teradataR)
    Sys.sleep(1)
    tdConnect(host,user,passwd,database,"jdbc")
}

if (exists("connection")) {
    ## reconnect to database (e.g. after VPN disconnects)
    cat(paste0("disconnecting old connection ..."))
    dbDisconnect(connection)
    rm(connection)
}

##
## following connection code takes care of this bug:
## need to add class path twice (recurring problem in MAC as of:
## http://forums.teradata.com/forum/analytics/connecting-to-teradata-in-r-via-the-teradatar-package
tryCatch({
    connection <<- myConnect()
},error=function(e)e,
finally = {
    Sys.sleep(3)
    connection <<- myConnect()
})

cat("done!\n")

#connection <- myConnect()
ls()
