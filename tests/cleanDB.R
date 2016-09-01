library("optparse")

option_list = list(
  make_option(c("-d", "--directory"),
              default=".", 
              help="directory where droptables.csv with info about tables to be dropped will be created.",
              type="character"),
  make_option(c("-H", "--host"),
                default="jdbc:teradata://10.200.4.116", 
                help="Host to connect to or odbj connection name [default= %default]",
                type="character"),
  make_option(c("-u", "--user"),
              default="", 
              help="credentials, user name [default= %default]",
              type="character"),
  make_option(c("-p", "--password"),
              default="", 
              help="password [default= %default]",
              type="character"),
  make_option(c("-D", "--database"),
              default="FL_demo,FL_train", 
              help="databases to clean.Comma separated. [default= %default]",
              type="character"),
  make_option(c("-J", "--jarDir"),
              default="/User/phani/Downloads", 
              help="directory with jar files to load [default= %default]",
              type="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

require(AdapteR)

cat(paste0("You requested to store droptables.csv in ",opt$directory,"\n"))
vdatabases <- unlist(strsplit(opt$database,","))
if(grepl("^jdbc",opt$host)){
    connection <-
        flConnect(
            host     = opt$host,
            database = vdatabases[1],
            user = opt$user,
            passwd = opt$password,
            ## set jdbc.jarsDir to add jdbc driver
            ## and security jars to classpath:
            ##    terajdbc4.jar tdgssconfig.jar
            ## CAVE: fully qualified PATH required
            jdbc.jarsDir = opt$jarDir,
            driverClass = NULL,
            debug=F,
            verbose=TRUE)
} else {
    connection <- flConnect(odbcSource=opt$host,
              database=opt$database,
              platform="TD")
}

vTables <- sqlQuery(connection,
                    paste0("SELECT databasename as db,\n ",
                           "tableName as name,\n ",
                           "tablekind as kind \n  ",
                           "FROM dbc.tables \n ",
                           "WHERE databaseName IN(",
                           paste0("'",vdatabases,"'",
                                 collapse=","),
                           ")"))

vtbl <- paste0(vTables[["db"]],".",vTables[["name"]])
names(vtbl) <- toupper(vTables[["kind"]])
##Consider only Tables and Views
vtbl <- vtbl[names(vtbl) %in% c("T","V")]
vtbl <- gsub("\\s+","",vtbl)
vReqtbls <- grep("\\.ARBase|\\.[a-z][0-9]",vtbl,value=TRUE)
vdf <- data.frame(kind=names(vReqtbls),tablename=vReqtbls)
write.csv(vdf,paste0(opt$directory,"/droptables.csv"))
cat("Completed manual checking of tables to be dropped from ",
    opt$directory,"/droptables.csv","?y/n:")
vtemp <- readLines(file("stdin"),1)
if(vtemp=="n"){
    cat("tables not dropped.Deleting droptables.csv..\n ")
}
if(vtemp=="y"){
    vdf <- read.csv(paste0(opt$directory,"/droptables.csv"))
    vReqtbls <- as.character(vdf[["tablename"]])
    vkind <- vdf[["kind"]]
    if(is.logical(vkind)){
        vkindCopy <- vkind
        vkind[vkindCopy]<-"T"
        vkind[!vkindCopy]<-"V"
    }
    names(vReqtbls) <- as.character(vkind)
    for(i in 1:length(vReqtbls)){
        vsqlstr <- paste0("DROP ",
                          ifelse(names(vReqtbls)[i]=="T","TABLE","VIEW"),
                          " ",vReqtbls[i],";")
        print(vsqlstr)
        vtemp <- sqlSendUpdate(connection,vsqlstr)
    }
}

cat("Deleting ",opt$directory,"/droptables.csv \n ")
system2("rm",paste0(opt$directory,"/droptables.csv"))
cat("......END...... \n ")
