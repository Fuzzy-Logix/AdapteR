require(MASS)
require(clusterGeneration)

set.seed(2)
num.vars<-15
num.obs<-200
cov.mat<-genPositiveDefMat(num.vars,covMethod="unifcorrmat")$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
parms<-runif(num.vars,-10,10)
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

Renv <- new.env(parent = globalenv())
Renv$tbl<-data.frame(y,rand.vars)

FLenv <- as.FL(Renv)
## formula 
vform <- as.formula(paste0("y~ ",paste0(names(FLenv$tbl)[3:length(names(FLenv$tbl))], collapse = "+")))
#' VIF values for independent variable.
flmod <- vif(vform,data = FLenv$tbl)
FLexpect_equal(nrow(flmod$vif),num.vars)
FLexpect_equal(flmod$vif$vif[1:5],c(27.73,
                                   36.89,
                                   12.56,
                                   50.73,
                                   8.35),
               tolerance = .01)


## for backward with threshold = 5
flmod <- vif(vform,data = FLenv$tbl, method = "bw", threshold  = 5)

FLexpect_equal(nrow(flmod$vif),65)
FLexpect_equal(length(flmod$select),11)
FLexpect_equal(flmod$select,c(1, 2,  3,  4,  5,  9, 10, 11, 13, 14, 15))
