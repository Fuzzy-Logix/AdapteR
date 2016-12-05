Renv=new.env(parent=globalenv())
FLenv=as.FL(Renv)
Renv$datalm<-data.frame(a=rnorm(100),
                   b=rnorm(100),
                   c=rnorm(100))
Renv$dataglm<-data.frame(a=rnorm(100),
                    b=rnorm(100),
                    c=sample(0:1,100,replace = T))
rownames(Renv$datalm)<-1:nrow(Renv$datalm)
rownames(Renv$dataglm)<-1:nrow(Renv$dataglm)
FLenv$datalm=as.FLTable(Renv$datalm,temporary=FALSE)
FLenv$dataglm=as.FLTable(Renv$dataglm,temporary=FALSE)

test_that("test for object structures and coefficients existence",{
  FLenv$linobj<-step(object =  FLenv$datalm,scope = c~a+b,familytype = "linear",direction="sf")
  FLenv$logobj<-step(object =  FLenv$dataglm,scope = c~a+b,familytype="logistic",direction="sf")
  Renv$linobj<-lm(Renv$datalm,formula = c~a+b)
  Renv$logobj<-glm(Renv$dataglm,formula = c~a+b,family = "poisson")
  
  result=expect_equal(rownames(coefficients(FLenv$linobj)) %in% colnames(Renv$datalm),rep(TRUE,nrow(FLenv$linobj$coefficients)))
  result2=expect_equal(rownames(coefficients(FLenv$logobj)) %in% colnames(Renv$dataglm),rep(TRUE,nrow(FLenv$logobj$coefficients)))
  result3=eval_expect_equal({
    x<-!is.null(coefficients(linobj))
    y<-!is.null(coefficients(logobj))
  },Renv,FLenv,
  )
})


