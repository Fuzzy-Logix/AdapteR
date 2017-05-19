Renv = new.env(parent = globalenv())

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
groupf <- gl(2, 10, 20, labels = c("Ctl","Trt"))
groupb = rep(0:1,each = 10)
weight <- c(ctl, trt)
dataframe = data.frame(weight = weight,groupf =groupf,groupb =groupb)
rownames(dataframe) <- 1:nrow(dataframe)
FLenv = as.FL(Renv)
Renv$dataframe <- dataframe
FLenv$dataframe <- as.FLTable(dataframe,temporary=F)

test_that("lm: execution",{
    result = eval_expect_equal({
             lmobj <- lm(weight ~ groupb,data=dataframe)
             C <- coef(lmobj)
    },Renv,FLenv,
    expectation="C",
    noexpectation = "lmobj",
    check.attributes=F)
})

test_that("lm: coefficient names https://app.asana.com/0/143316600934101/156989386834241",{
    result = eval_expect_equal({
    },Renv,FLenv,
    expectation="C")
})


test_that("lm: equality of coefficients, residuals, fitted.values, rank and terms",{
    result = eval_expect_equal({
        mycoefffs <- lmobj$coefficients
        myres <- as.vector(lmobj$residuals)
        myfitted.values <- as.vector(lmobj$fitted.values)
        names(myres) <- names(myfitted.values) <- NULL ## todo: support names in AdapteR
        mydf.res <- lmobj$df.residual
        myrank <- lmobj$rank
        myterms <- lmobj$terms
        modelDim <- dim(lmobj$model)
    },Renv,FLenv)
})


test_that("lm: support of factors, https://app.asana.com/0/143316600934101/146934264360575",{
    result = eval_expect_equal({
      lmobj <- lm(weight ~ groupf,data=dataframe)
      C <- coef(lmobj)
    },Renv,FLenv,
    expectation="C",
    noexpectation = "lmobj")
})

test_that("lm: execution with x=TRUE,y=TRUE",{
    result = eval_expect_equal({
             lmobj <- lm(weight ~ groupb,data=dataframe,x=TRUE,y=TRUE)
             xDim <- dim(lmobj$x)
             ylength <- length(lmobj$y)
    },Renv,FLenv,
    noexpectation = "lmobj")
})

test_that("lm: summary.lm https://app.asana.com/0/143316600934101/156948192818458",{
  result = eval_expect_equal({
    lmSum <- summary(lmobj)
  },Renv,FLenv,
  noexpectation = "lmSum")
})

## Check for plot function of Linear Regression.
## 
## check to run manually for equal results
if(FALSE){
  plot(Renv$lmobj,las= 1)
  plot(FLenv$lmobj)
}


## MD Testing
## Tables not available in Hadoop
## FLRegrDataPrepMD is not available in Hadoop and Aster
test_that("lm: multi dataset ",{
    flMDObject <- FLTableMD(table=getTestTableName("tblAutoMPGMD"),
                            group_id_colname="GroupID",
                            obs_id_colname="ObsID",group_id = c(2,4))

    flMDObjectDeep <- FLTableMD(table=getTestTableName("LinRegrMultiMD"),
                                group_id_colname="DatasetID",
                                obs_id_colname="ObsID",
                                var_id_colname="VarID",
                                cell_val_colname="Num_Val")

    vformula <- MPG~HorsePower+Displacement+Weight+Acceleration

    lmfit <- lm(vformula,
                data=flMDObject)
    coeffList <- coef(lmfit)
    summaryList <- summary(lmfit)
    test_that("Check for dimensions of coefficients and summary for wideTable ",{
        expect_equal(names(coeffList),
                     paste0("Model",flMDObject@Dimnames[[3]]))
        expect_equal(names(coeffList),
                     names(summaryList))
        vcoeffnames <- all.vars(vformula)
        vcoeffnames <- c("(Intercept)",
                         vcoeffnames[2:length(vcoeffnames)])
        lapply(coeffList,function(x){
            expect_equal(names(x),vcoeffnames)
        })
    })

    lmfit <- lm(vformula,
                data=flMDObjectDeep)
    coeffList <- coef(lmfit)
    summaryList <- summary(lmfit)
    test_that("Check for dimensions of coefficients and summary for DeepTable ",{
        expect_equal(names(coeffList),
                     paste0("Model",flMDObjectDeep@Dimnames[[3]]))
        expect_equal(names(coeffList),
                     names(summaryList))
        vlenCoeffs <- colnames(flMDObjectDeep)[[1]]
        vlenCoeffs <- length(setdiff(vlenCoeffs[1]:vlenCoeffs[2],-1))
        lapply(coeffList,function(x){
            expect_equal(length(x),vlenCoeffs)
        })
    })
})

## Testing lm for non-continuous ObsIDs
widetable  <- FLTable(getTestTableName("tblAbaloneWide"),
                    "obsid",whereconditions=c("ObsID>10","ObsID<1001"))
if(is.TDAster()) vformula <- rings~height+diameter else vformula <- Rings~Height+Diameter
object <- lm(vformula,widetable)

test_that("Check for dimensions of x Matrix ",{
        expect_equal(nrow(object$x),nrow(widetable))
        expect_equal(colnames(object$x),
                    c("(Intercept)","Height","Diameter"))
        expect_equal(rownames(object$model),
                     as.character(rownames(widetable))
                    )
        expect_equal(colnames(object$model),
                     c("Rings","Height","Diameter")
                    )
})

test_that("Check for dimensions of x Matrix ",{
    deeptable <- FLTable(getTestTableName("myLinRegrSmall"),
                         "ObsID","VarID","Num_Val",
                         whereconditions=c("ObsID>10","ObsID<1001"))
    object <- lm(NULL,deeptable)
    expect_equal(nrow(object$x),nrow(deeptable))
    expect_equal(colnames(object$x),
                 c("(Intercept)",paste0("var",colnames(deeptable)[c(-1,-2)])))
    expect_equal(dimnames(object$model),
                 list(as.character(rownames(deeptable)),
                      c("varY",paste0("var",colnames(deeptable)[c(-1,-2)]))
                      ))
})
