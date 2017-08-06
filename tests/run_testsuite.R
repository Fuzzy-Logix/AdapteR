library(optparse)

option_list = list(
    make_option(c("-d", "--directory"),
                default=".", 
                help="subdirectory to test, if '.', assumes to be run from tests directory [default= %default]",
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
                default="FL_TRAIN", 
                help="database [default= %default]",
                type="character"),
    make_option(c("-c", "--dropTables"),
                action="store_true",
                default="TRUE", 
                help="drop AdapteR tables when starting the session [default= %default]",
                type="character"),
    make_option(c("-t", "--temporary"),
                action="store_true",
                default="FALSE", 
                help="temporary session [default= %default]",
                type="character"),
    make_option(c("-A", "--AdapteR"),
                default=".", 
                help="if 'require' load installed AdapteR version, otherwise load from git repository provided [default= %default]",
                type="character"),
    make_option(c("-J", "--jarDir"),
                default="~/SecuriSync/AdapteR 2015/Connectors/jdbc drivers/Teradata", 
                help="directory with jar files to load [default= %default]",
                type="character"),
    make_option(c("-P", "--platform"),
                default="TD",
                help="TDAster,TD and Hadoop supported",
                type="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)


if(opt$directory=="."){
    opt$directory <- gsub("/tests$","",getwd())
} 

packagedir <- opt$directory
print(opt$directory)

# packagedir <- gsub("^.*/","",opt$directory)
# basedir <- gsub("/[^/]*$","",opt$directory)
# print(packagedir)
# print(basedir)

# cat(paste0("You requested to run tests in ",opt$directory,"\nTrying to go to directory\ncd ",basedir,"\nand build and test package\n",packagedir,"\n"))
if(opt$AdapteR=="require"){
    if(!grepl("^jdbc",opt$host))
        library(RODBC)
    ##phani: do we need to set this?
    # setwd(basedir)
    require("AdapteR")
} else {
    if(!grepl("^jdbc",opt$host))
        library(RODBC)
    ##phani: I think git pull should be done outside the sript
    ## As this might change my working branch
    # cat(paste0("running git pull\n"))
    # setwd(basedir)
    # setwd(packagedir)
    # system2("git", c("pull", "fuzzylogix", "master"),stdout = TRUE)
    # setwd(basedir)
    devtools::document(packagedir)
    devtools::load_all(packagedir,export_all = FALSE)
}

if(opt$dropTables=="FALSE"){
    vdrop=FALSE
}else vdrop=TRUE

if(opt$temporary=="TRUE"){
    vtemp=TRUE
}else vtemp=FALSE

if(grepl("^jdbc",opt$host)){
    connection <-
        flConnect(
            host     = opt$host,
            database = opt$database,
            user = opt$user,
            passwd = opt$password,
            ## set jdbc.jarsDir to add jdbc driver
            ## and security jars to classpath:
            ##    terajdbc4.jar tdgssconfig.jar
            ## CAVE: fully qualified PATH required
            jdbc.jarsDir = opt$jarDir,
            debug=T,
            drop=vdrop,
            verbose=TRUE,
            temporary=vtemp,
            pkg="dbc")
            # TestDatabase=opt$database)
} else {
    options(debugSQL=TRUE)
    connection <- flConnect(odbcSource=opt$host,
                          database=opt$database,
                          platform=opt$platform,
                          drop=vdrop,
                          temporary=vtemp,
                          pkg="dbc"
                          # TestDatabase=opt$database
                          )
}

## check if connection is working:
sqlQuery(connection,paste0("SELECT * FROM ",getTestTableName("tblmatrixmulti"),
                            " WHERE matrix_id=1 "))
setupls <- ls()
setwd(paste0(packagedir,"/tests"))
cat(paste0("Running tests from tests directory",getwd(),"\n"))

source("alltests.R")
rm(list=setupls)
ls()
sha <- system2("git", c("log", "--pretty=format:'%h'", "-n","1"),stdout = TRUE)
branch <- gsub("^.*/","",system2("git", c("symbolic-ref", "HEAD"),stdout = TRUE))
save.image(file=paste0("alltests_",Sys.Date(),"_",branch,"_",sha,".rda"))
setwd("..")
