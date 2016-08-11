parts <- c("coefficients","residuals",
           "fitted.values","df.residual" )
fam <- "binomial"

Renv = new.env(parent = globalenv())

var1 <- rnorm(200)
var2 <- rnorm(200)
var3 <- sample( c(0, 1), 200, replace = TRUE)
dataf<- data.frame(var1 = var1,var2 =var2, var3 = var3,offset=1)
#rownames(var4) <- 1:nrow(var4)
Renv$dataf <- dataf
FLenv = as.FL(Renv)


test_that("glm: execution for binomial ",{
  result = eval_expect_equal({
    glmobj <- glm(var3 ~ var1 + var2, data=dataf, family = fam)

    assign(paste0(fam, "coeff") , coef(glmobj), env = parent.env(environment()))
                    
           },Renv,FLenv,
           expectation = "binomialcoeff",
           noexpectation = "glmobj",
           check.attributes=F,
           tolerance = .000001,
           verbose = T
          
           )
         }) 



test_that("glm: equality of coefficients, residuals, fitted.values, df.residual for binomial",{
  result = eval_expect_equal({
    glmobj <- glm(var3 ~ var1 + var2, data=dataf, family = fam)

    sapply(parts,
           function(i){

             
             assign(paste0("glmobj", i) , do.call("$", list(glmobj, i)), env = parent.env(environment()))
             #print(get(paste0(z, i)))
           })
      
   # modelDim <- dim(glmobj$model)
  },Renv,FLenv,
  noexpectation = "glmobj",
  expectation = lapply(parts, function(i){paste0("glmobj", i)}),
  tolerance = .000001,
  check.attribute = F,
  verbose = T
)

})




#summary, plot??