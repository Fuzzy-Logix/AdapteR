
parts <- c("coefficients","residuals",
           "fitted.values","df.residual"           )
fam <- c("binomial", "poisson")

Renv = new.env(parent = globalenv())

var1 <- rnorm(2000)
var2 <- rnorm(2000)
var3 <- sample( c(0, 1), 2000, replace = TRUE)
dataf<- data.frame(var1 = var1,var2 =var2, var3 = var3)
#rownames(var4) <- 1:nrow(var4)
Renv$dataf <- dataf
FLenv = as.FL(Renv)



#Error: Difference of 1 in Poisson coefficients


test_that("glm: execution for binomial and poisson",{
  result = eval_expect_equal({
    sapply(fam, 
           function(z){
             glmobj <- glm(var3 ~ var1 + var2,data=dataf, family = z)
             assign(paste0(z, "coeff") , do.call("coef", list(glmobj)), env = parent.env(environment()))
                    })
           },Renv,FLenv,
           noexpectation = "glmobj",
           check.attributes=F,
           tolerance = .000001
           )
         }) 
         
        

# tests for poisson family of glm

test_that("glm: equality of coefficients, residuals, fitted.values, df.residual for poisson: https://app.asana.com/0/143316600934101/158507710191276",{
  result = eval_expect_equal({
    glmpois <- glm(var3 ~ var1 + var2,data=dataf, family = poisson)
    sapply(parts,
           function(i){

             assign(paste0("glmpois", i) , do.call("$", list(glmpois, i)), env = parent.env(environment()))
             #print(get(paste0(z, i)))
           })
      
   # modelDim <- dim(glmobj$model)
  },Renv,FLenv,
  noexpectation = "glmpois",
  tolerance = .000001,
  check.attribute = F
)

})

#tests for binomial family of glm

test_that("glm: equality of coefficients, residuals, fitted.values, df.residual for binomial: https://app.asana.com/0/143316600934101/158092657328844",{
  result = eval_expect_equal({
    glmbinom <- glm(var3 ~ var1 + var2, data=dataf, family = binomial)
    sapply(parts,
           function(i){

             assign(paste0("glmbinom", i) , do.call("$", list(glmbinom, i)), env = parent.env(environment()))
             #print(get(paste0(z, i)))
           })
      
   # modelDim <- dim(glmobj$model)
  },Renv,FLenv,
  noexpectation = "glmbinom",
  tolerance = .000001,
  check.attribute = F
)

})





test_that("glm: summary https://app.asana.com/0/143316600934101/157975488977413",{
  result = eval_expect_equal({
    sapply(fam, function(z){
                  glmobj <- glm(var3 ~ var1 + var2,data=dataf, family = z)
                  assign(paste0(z, "summary") , do.call("summary", list(glmobj)), env = parent.env(environment()))
                  })
                  
  },Renv,FLenv,
  noexpectation = "glmobj" )
})

## Check for plot function of glm.
## ??
## check to run manually for equal results
