library("optparse")

option_list = list(
    make_option(c("-d", "--directory"),
                default="/Users/gregor/fuzzylogix/AdapteR/AdapteR", 
                help="subdirectory to test [default= %default]",
                type="character"),
    make_option(c("-H", "--host"),
                default="jdbc:teradata://10.200.4.116", 
                help="subdirectory to test [default= %default]",
                type="character"),
    make_option(c("-u", "--user"),
                default="", 
                help="subdirectory to test [default= %default]",
                type="character"),
    make_option(c("-p", "--password"),
                default="", 
                help="subdirectory to test [default= %default]",
                type="character"),
    make_option(c("-D", "--database"),
                default="Fl_demo", 
                help="subdirectory to test [default= %default]",
                type="character"),
    make_option(c("-J", "--jarDir"),
                default="/Users/gregor/fuzzylogix/Teradata/jdbc", 
                help="subdirectory to test [default= %default]",
                type="character")
)


opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

packagedir <- gsub("^.*/","",opt$directory)
basedir <- gsub("/[^/]*$","",opt$directory)
setwd(basedir)
devtools::document(packagedir)
devtools::load_all(packagedir)

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
        driverClass = "com.teradata.jdbc.TeraDriver",
        debug=T,
        verbose=TRUE)
require(testthat)

setupls <- ls()

source(paste0(opt$directory,"/tests/alltests.R"))

setwd(opt$directory)
rm(list=setupls)
ls()
sha <- system2("git", c("log", "--pretty=format:'%h'", "-n","1"),stdout = TRUE)
branch <- gsub("^.*/","",system2("git", c("symbolic-ref", "HEAD"),stdout = TRUE))
save.image(file=paste0("alltests_",Sys.Date(),"_",branch,"_",sha,".rda"))
setwd("..")
