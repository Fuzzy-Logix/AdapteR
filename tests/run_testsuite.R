library("optparse")

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
                default="Fl_demo", 
                help="database [default= %default]",
                type="character"),
    make_option(c("-A", "--AdapteR"),
                default="require", 
                help="if 'require' load installed AdapteR version, otherwise load from git repository in --directoy [default= %default]",
                type="character"),
    make_option(c("-J", "--jarDir"),
                default="/Users/gregor/fuzzylogix/Teradata/jdbc", 
                help="directory with jar files to load [default= %default]",
                type="character")
)


opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)


if(opt$directory=="."){
    opt$directory <- gsub("/tests$","",getwd())
} 

packagedir <- gsub("^.*/","",opt$directory)
basedir <- gsub("/[^/]*$","",opt$directory)

cat(paste0("You requested to run tests in ",opt$directory,"\nTrying to go to directory\ncd ",basedir,"\nand build and test package\n",packagedir,"\n"))
if(opt$AdapteR=="require"){
    setwd(basedir)
    require("AdapteR")
} else {
    cat(paste0("running git pull\n"))
    setwd(basedir)
    setwd(packagedir)
    system2("git", c("pull", "fuzzylogix", "master"),stdout = TRUE)
    setwd(basedir)
    devtools::load_all(packagedir)
}

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
            driverClass = "com.teradata.jdbc.TeraDriver",
            debug=T,
            verbose=TRUE)
} else {
    connection <- flConnect(odbcSource=opt$host,
              database=opt$database,
              platform="TD")
}

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
