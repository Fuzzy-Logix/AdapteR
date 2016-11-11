library(rpart)

Renv=new.env(parent= globalenv())
FLenv= as.FL(Renv)
Renv$kyphosis<-kyphosis
colnames(Renv$kyphosis)<-paste0("Col",1:ncol(kyphosis))
FLenv$kyphosis<-as.FLTable(Renv$kyphosis,temporary=FALSE)

FLenv$deeptable<-FLTable("tblDecisionTreeMulti","ObsID","VarID","Num_Val")
Renv$deeptable<-as.data.frame(FLenv$deeptable)

test_that("FLrpart: test for deep tables",{
	robj<-rpart(Renv$deeptable,formula= Renv$deeptable$`-1`~.,method="class")
	flobj<-FLrpart(FLenv$deeptable,formula= -1~.)
	FLexpect_equal(robj,flobj)
})

str(robj)
str(flobj)

test_that("FLrpart: test for wide tables",{
	robj<-rpart(Renv$kyphosis,formula= Col1~.)
	flobj<-FLrpart(FLenv$kyphosis, formula=Col1~.)
	FLexpect_equal(robj,flobj)
})


## -  read the vignette
## -  change FLrpart from S4 to S3 class (or implement $ if you have very good reasons)
## -  inspect workings of rpart summary
##    getAnywhere("summary.rpart")
##    getAnywhere("print.rpart")
## -  implement all $ accessors for stuff used in rpart summary
##    (as much as possible)
##    
## -  construct result such that all the 
        cuts <- character(nrow(x$splits))
        temp <- x$splits[, 2L]
        for (i in seq_along(cuts)) {
            cuts[i] <- if (temp[i] == -1L) 
                paste("<", format(signif(x$splits[i, 4L], digits)))
            else if (temp[i] == 1L) 
                paste("<", format(signif(x$splits[i, 4L], digits)))
            else paste("splits as ", paste(c("L", "-", "R")[x$csplit[x$splits[i, 
                4L], 1:temp[i]]], collapse = "", sep = ""), collapse = "")
        }
        if (any(temp < 2L)) 
            cuts[temp < 2L] <- format(cuts[temp < 2L], justify = "left")
        cuts <- paste0(cuts, ifelse(temp >= 2L, ",", ifelse(temp == 
            1L, " to the right,", " to the left, ")))


            cat("  Primary splits:\n")
            j <- seq(index[i], length.out = 1L + ff$ncompete[i])
            temp <- if (all(nchar(cuts[j], "w") < 25L)) 
                format(cuts[j], justify = "left")
            else cuts[j]
            cat(paste("      ", format(sname[j], justify = "left"), 
                " ", temp, " improve=", format(signif(x$splits[j, 
                  3L], digits)), ", (", nn - x$splits[j, 1L], 
                " missing)", sep = ""), sep = "\n")
