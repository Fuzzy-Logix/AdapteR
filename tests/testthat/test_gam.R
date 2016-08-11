## test for gam function

parts <- c("coefficients","residuals",
           "fitted.values","df.residual")

connection <- flConnect(odbcSource = "TDVM",database = "fuzzylogix",platform="TD")

Renv<-new.env(parent=globalenv())

FLenv<-as.FL(Renv)

FLenv$widetable <- FLTable("fuzzylogix","tblGAMSimData","ObsID")
Renv$table<-as.data.frame(FLenv$widetable)

colnames(Renv$table)<-colnames(FLenv$widetable)


myformula <- yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)
myformula2<-offset(x2Val)+yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)

FLenv$gamobject <- gam(myformula,data=FLenv$widetable,offset="x2Val")
Renv$gamobject<- gam(myformula2,data=Renv$table)


## gam function doesn't work
## asana ticket- https://app.asana.com/0/143316600934101/158507710191280


test_that("test for gam",{
  
  result = eval_expect_equal( {
    
    a<-sapply(parts,
           function(i){
             assign(i,do.call("$",list(gamobject,i)))
          }        
             ) 
    
  },Renv,FLenv,
    noexpectation = "gamobject",
    expectation = "a"
    
  )
})
