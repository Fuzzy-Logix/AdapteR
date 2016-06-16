#ERRORS   asana tickets- https://app.asana.com/0/143316600934101/144942913968262
#                        https://app.asana.com/0/143316600934101/144952239565760 
Renv <- new.env(parent=globalenv())
library(testthat)

Renv$txt <- c("arm","foot","lefroo", "bafoobar")


test_that("String functions",{
  eval_expect_equal({
    test1 <- grep("[a-z]", letters)},Renv)})


test_that("String functions",{
          eval_expect_equal({
            #txt <- c("arm","foot","lefroo", "bafoobar")
            if(length(i <- grep("foo", txt)))
              
              #txt converted to character to print in cat()
              cat("'foo' appears at least once in\n\t", as.character(txt), "\n")  
            
            },Renv)})



Renv$txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")


#Error value=TRUE not considered    asana - https://app.asana.com/0/143316600934101/144952239565760
test_that("String functions",{
            eval_expect_equal({
            ( i <- grep("[gu]", txt) ) # indices
            stopifnot( txt[i] == grep("[gu]", txt, value = TRUE) )},Renv)})


#ERROR NO INHERITED METHOD FOR FUNCTION as.FL FOR SIGNATURE "FUNCTION"  asana - https://app.asana.com/0/143316600934101/144942913968262
test_that("String functions",{
          eval_expect_equal({
## Using grepl() for filtering
## Find functions with argument names matching "warn":
                findArgs <- function(env, pattern) {
                nms <- ls(envir = as.environment(env))
                nms <- nms[is.na(match(nms, c("F","T")))] # <-- work around "checking hack"
                aa <- sapply(nms, function(.) { o <- get(.)
                if(is.function(o)) names(formals(o)) })
                  iw <- sapply(aa, function(a) any(grepl(pattern, a, ignore.case=TRUE)))
              aa[iw]}
              findArgs("package:base", "warn")},Renv)})





  
  
  
  
  
  




