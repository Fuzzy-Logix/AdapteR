demos <- list.files(path="../demo",pattern=".*\\.R$",full.names = TRUE)
oreadline <- readline
for(f in demos){
    test_that(paste0("demo ", gsub("^.*/","",f), "runs"), {
        readline <- function(x) cat(paste0(x," \n"))
        source(f)
    })
}
readline <- oreadline
options(debugSQL=F)
