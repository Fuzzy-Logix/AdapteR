Renv = new.env(parent = globalenv())
Renv$train = rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
Renv$test = rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
Renv$cl = c(rep(1,25), rep(2,25), rep(3,25))

FLenv <- as.FL(Renv)

Renv$length2 <- nrow(Renv$test)
FLenv$length2 <- length(knn(FLenv$train, FLenv$test, FLenv$cl, k = 3, classify=FALSE))

test_that("knn classification: Result correctness",{
    result = eval_expect_equal({
        result <- knn(train, test, cl, k = 3, prob=TRUE)
        result1 <- as.integer(as.vector(result))
    }, Renv, FLenv,
    expectation="result1",
    noexpectation="result")
})

# # Run the following script to examine the errors generated::
# # Errors in Error in class::knn's implementation
# # checking script::
# ## Create dist matrices in R and FL:
# Renv$rdist <- dist(rbind(Renv$test,Renv$train))

# Renv$dist2matrix <- function(pDist,
#                               upper=FALSE,
#                               diag=FALSE){
#                             if(is.FL(pDist))
#                               return(pDist)
#                             vsize <- attr(pDist,"Size")
#                             vmat <- matrix(0,vsize,vsize)
#                             ventries <- as.vector(pDist)
#                             k <- 1
#                             for(i in 1:(vsize-1))
#                               for(j in (i+1):vsize){
#                                 vmat[i,j] <- ventries[k]
#                                 k <- k+1
#                               }
#                             if(upper) vmat+t(vmat)
#                             else t(vmat)
#                         }
# Renv$rrdist <- Renv$dist2matrix(Renv$rdist,upper=TRUE)
# Renv$dist <- Renv$rrdist[1:75,76:150]

# FLenv$dist <- as.matrix(FLgetDistMatrix(FLenv$test,FLenv$train,upper=TRUE,diag=TRUE))

# test_that("Save dist matrices into excel files.. please delete these manually after checking... ",{
#   result = eval_expect_equal({
#     library(xlsx)
#     getEnvName <- function(pEnv){
#       if(identical(pEnv,Renv))
#         venvName <- "Renv"
#       else venvName <- "FLenv"
#       return(venvName)
#     }
#     t <- write.xlsx(dist,"KNNresultCheckDistMatrices.xlsx",
#                     paste0(getEnvName(environment()),"Dist"),
#                     append=TRUE)
#   }, Renv, FLenv)
# })

# # # Examine the failing cases
# # # Examine the result correctness in both environments
# test_that("knn classification: Testing Result correctness",{
#   result = eval_expect_equal({
#     for(i in which(!Renv$result1==as.vector(FLenv$result1))){
#       cat("Observation: ",i," \n ")
#       venvName <- getEnvName(environment())
#       # cat("Evaluation in ",base::environmentName(environment()),":: \n ") ## Not working!
#       cat("Evaluation in ",venvName,":: \n ")
#       cat("Nearest neighbour: ",which.min(dist[i,])," \n ")
#       cat("Minimum Distance: ",min(dist[i,])," \n ")
#       cat("Expected: ",as.vector(cl)[which.min(dist[i,])]," \n ")
#       cat("Returned: ",as.vector(result1)[i]," \n ")
#       cat("\n \n \n ")
#     }
#   }, Renv, FLenv,
#   noexpectation = "venvName")
# })


test_that("knn classification: prob attribute working",{
    result = eval_expect_equal({
        attr(result,"prob")
    }, Renv, FLenv)
})

test_that("knn Regression: check working",{
    result = eval_expect_equal({
    }, Renv, FLenv,
    expectation="length2")
})

