library(AdapteR)
library(testthat)

test_that("check_crossprod",{
    m<-initF.FLMatrix(n=5,isSquare = FALSE)
    m1<-initF.FLMatrix(n=5,isSquare = TRUE)
    v<-initF.FLVector(n=6,isRowVec = TRUE)
    v1<-initF.FLVector(n=6,isRowVec = FALSE)
    
    object<-crossprod(m$FL,m1$FL)
    expected<-crossprod(m$R,m1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$FL,v$FL)
    expected<-crossprod(m$R,v$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$FL,v$R)
    expected<-crossprod(m$R,v$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$FL,m1$R)
    expected<-crossprod(m$R,m1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(v1$FL,m1$FL)
    expected<-crossprod(v1$R,m1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(v1$FL,v$FL)
    expected<-crossprod(v1$R,v$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(v1$FL,m1$R)
    expected<-crossprod(v1$R,m1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(v1$FL,v$R)
    expected<-crossprod(v1$R,v$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$R,m1$FL)
    expected<-crossprod(m$R,m1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$R,v1$FL)
    expected<-crossprod(m$R,v1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$R,m1$R)
    expected<-crossprod(m$R,m1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    object<-crossprod(m$R,v1$R)
    expected<-crossprod(m$R,v1$R)
    FLexpect_equal(object,expected,check.attributes = FALSE)

    })