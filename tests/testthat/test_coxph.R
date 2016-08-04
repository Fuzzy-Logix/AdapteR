
parts <- c("nevent", "n", "wald.test", "means", "loglik")
data(lung)
lung1 <- lung
lung1$status <- lung1$status - 1
Renv = new.env(parent = globalenv())
#fit <- coxph(Surv(time, status) ~ age + sex, lung1) 


Renv$lung1 <- lung1
FLenv = as.FL(Renv)



# options(debugSQL = FALSE)

test_that("cox coefficients  https://app.asana.com/0/136555696724838/163682320948854 ",{
  result = eval_expect_equal({
#    print(ls())
 #   print(any(ls() == "coxobj"))
    if(!any(ls() == "coxobj"))
    {
      coxobj <- coxph(Surv(time, status) ~ age + sex, lung1)
    }
    
    
    assign("coeffcox" , coxobj$coefficients)
    
      
    
    
    
  },Renv,FLenv,
  expectation = "coeffcox",
  noexpectation = c("coxobj","lung1"),
  check.attributes=F,
  tolerance = .000001,
  verbose = T
  )
}) 



test_that("cox: equality of  nevent, n, waldscore, means,linear.predictors https://app.asana.com/0/136555696724838/163682320948854 ",{
  result = eval_expect_equal({
    if(!any(ls() == "coxobj"))
    {
      coxobj <- coxph(Surv(time, status) ~ age + sex, lung1)
    }
    
    sapply(parts,
           function(i){
             
             
             assign(paste0("coxobj", i) , as.numeric(do.call("$", list(coxobj, i))), env = parent.env(environment()))
             #print(get(paste0(z, i)))
           })
    
    # modelDim <- dim(glmobj$model)
  },Renv,FLenv,
  noexpectation = "coxobj",
  expectation = lapply(parts, function(i){paste0("coxobj", i)}),
  tolerance = .000001,
  check.attribute = F,
  verbose = T
  )
  
})




test_that("cox linear.predictors  https://app.asana.com/0/136555696724838/163682320948854 ",{
  result = eval_expect_equal({
    if(!any(ls() == "coxobj")){
      coxobj <- coxph(Surv(time, status) ~ age + sex, lung1) 
    }
    
    
    
    
    assign("coefflin" , coxobj$linear.predictors)
    
  },Renv,FLenv,
  expectation = "coefflin",
  noexpectation = c("coxobj","lung1"),
  check.attributes=F,
  tolerance = .000001,
  verbose = T
  
  )
})















#summary, plot??