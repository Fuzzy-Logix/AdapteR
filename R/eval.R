## var <-lapply(ftocompare, function(i){paste0("FLexpect_equal(",i,"(rObject),",i,"(flObject))")})
## eval(parse(text = ))
## example
#'rm(list = setdiff(ls(),"connection"))
#'FLenv <- new.env(parent = globalenv())
#'Renv <- new.env(parent = globalenv())
#'FLenv$vec <- 1:5
#'Renv$vec <- 1:7
#'
#'
#'
#'test_that("AIC, LogLik:", {eval_dim_equal({
#'    vect <- vec + 1
#'    
#'},Renv,FLenv,
#'verbose = TRUE,
#'check.attributes = FALSE,
#'expectation = c("vec"),
#'ftocompare =c ("length", "names"))
#'})

eval_dim_equal <- function(e, Renv, FLenv,
                           description=NULL,
                           expectation=c(),
                           noexpectation=c(),
                           verbose=FALSE,
                           ftocompare = NULL,
                           ...){
    ##browser()
    e <- substitute(e)
    if(is.null(description)) description <- paste(deparse(e),collapse="\n")
    vdiff <- diffnam(e, Renv, FLenv,
                           expectation=expectation,
                           noexpectation=noexpectation,
                     verbose=verbose, ...)
    ftocompare <- c(ftocompare, "length","names")
    var <-lapply(unique(ftocompare),
                 function(i){paste0("compare(",i,"(rObject),",i,"(flObject))")})

    

    for(n in unique(vdiff$vnam)){
        rObject <- get(n,envir = Renv)
        flObject <- get(n,envir = FLenv)
        if(verbose) {
            cat(paste0("---------\n Testing for dimension: ",n,"\n R:\n"))
            str(rObject)
            cat(paste0(" FL:\n"))
            str(flObject)
            
        }
        lapply(unlist(var), function(i){
            print(i)
            print(eval(parse(text = i)))
            cat("\n")            
        })

        ##FLexpect_equal(length(rObject), length(flObject),label=n,...)
    }
    return(data.frame(description  = description,
                      r.Runtime    = vdiff$r.Runtime,
                      fl.Runtime   = vdiff$fl.Runtime))
}




diffnam <- function(e, Renv, FLenv,
                    expectation=c(),
                    noexpectation=c(),
                    ...)
{
    oldNames <- ls(envir = Renv)
    rStartT <- Sys.time()
    re <- tryCatch({
        eval(expr = e, envir=Renv)
        NULL
    }, error=function(err) {
        print(err)
        err
    })
    rEndT <- Sys.time()
    flStartT <- Sys.time()
    fle <- tryCatch({
        eval(expr = e, envir=FLenv)
        NULL
    }, error=function(err) {
        print(err)
        err
    })
    flEndT <- Sys.time()
    if(is.null(re))
        expect_null(fle,label=fle)
    ##expect_equal(e,fle)
    newNames <- ls(envir = Renv)
    vToCheckNames <- setdiff(newNames,oldNames)
    if(length(noexpectation)>0)
        vToCheckNames <- setdiff(vToCheckNames,noexpectation)
    if(length(expectation)>0)
        vToCheckNames <- c(expectation,vToCheckNames)

    return(list(vnam = vToCheckNames,
                r.Runtime    = rEndT-rStartT,
                fl.Runtime   = flEndT-flStartT )) }
