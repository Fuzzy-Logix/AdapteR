## demo("connecting")

############################################################
## First you pull all variable / data setup out of the example:
Renv <- new.env(parent = globalenv())
Renv$a <- 1:10
Renv$b <- 1:5

FLenv <- as.FL(Renv)


## if you want to use initF, you do it this way:
tmp <- initF.FLMatrix(10,TRUE)
Renv$bigobject <- tmp$R
FLenv$bigobject <- tmp$FL

############################################################
## R documentation example from stats::cor
## run expectations within test_that as blocks
test_that("Variance single column.",
          eval_expect_equal({
              vara <- var(a)  # 9.166667
              length(a)
          }, Renv, FLenv))

test_that("Correlation of two vectors. https://app.asana.com/0/136555696724838/143778401455751",{
    eval_expect_equal({
        varb <- var(b, b) # 2.5
    }, Renv, FLenv)
})

## TODO: change more from here
test_that("Correlation examples -- data needs pulling out!",{
    eval_expect_equal({
        ## Two simple vectors
        cor(1:10, 2:11) # == 1
    }, Renv, FLenv)
})



## Testing FLCorrel
##Failing because of precision errors.
test_that("check FLCorrel result",
{
    if(is.Hadoop())
        fltDeep <- FLTable(getTestTableName("tblUSArrests"),
                       "ObsID","DimID","Value", whereconditions = "ObsID<21")
    else 
    fltDeep <- FLTable(getTestTableName("tblUSArrests"),
                       "ObsID","VarID","Num_Val", whereconditions = "ObsID<21")
    RtDeep <- as.data.frame(fltDeep)

    if(is.Hadoop())
        fltWide <- FLTable(getTestTableName("tblUSArrests"),
                       "ObsID",whereconditions = c("DimID=2","ObsID<21"))
    else 
        fltWide <- FLTable(getTestTableName("tblUSArrests"),
                       "ObsID",whereconditions = c("DimID=2","ObsID<21"))
    RtWide <- as.data.frame(fltWide)
    vRow <- initF.FLVector(20,TRUE)
    flvRow <- vRow$FL
    RvRow <- vRow$R
    RvCol <- rnorm(20)
    flvCol <- as.FLVector(RvCol)
    m <- initF.FLMatrix(20)
    flm <- m$FL
    Rm <- m$R
    FLexpect_equal(cor(flm,flm),cor(Rm,Rm),check.attributes=FALSE)
    FLexpect_equal(cor(flvRow,flvRow),cor(RvRow,RvRow),check.attributes=FALSE)
    FLexpect_equal(cor(flvCol,flvCol),cor(RvCol,RvCol),check.attributes=FALSE)
    FLexpect_equal(cor(fltDeep,fltDeep),cor(RtDeep,RtDeep),check.attributes=FALSE)
    FLexpect_equal(cor(flm,flvRow),cor(Rm,RvRow),check.attributes=FALSE)
    FLexpect_equal(cor(flm,flvCol),cor(Rm,RvCol),check.attributes=FALSE, tolerance = 1e-7)
    FLexpect_equal(cor(flvCol,flvRow),cor(RvCol,RvRow),check.attributes=FALSE)
    FLexpect_equal(cor(flvRow,fltDeep),cor(RvRow,RtDeep),check.attributes=FALSE)
    FLexpect_equal(cor(flvCol,fltDeep),cor(RvCol,RtDeep),check.attributes=FALSE, tolerance = 1e-7)
    cor(fltDeep,fltWide)
    cor(fltWide,fltWide)
    cor(fltWide,fltDeep)
    cor(flm,fltWide)
    cor(flvRow,fltWide)
    cor(flvCol,fltWide)
})
