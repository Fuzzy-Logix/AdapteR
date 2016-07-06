
Renv = new.env(parent = globalenv())

Renv$text1 = letters
Renv$text2 = c("arm","foot","lefroo", "bafoobar")
Renv$text3 = c("The", "licenses", "for", "most", "software", "are",
  "designed", "to", "take", "away", "your", "freedom",
  "to", "share", "and", "change", "it.",
   "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
   "is", "intended", "to", "guarantee", "your", "freedom", "to",
   "share", "and", "change", "free", "software", "--",
   "to", "make", "sure", "the", "software", "is",
   "free", "for", "all", "its", "users")
Renv$text4 =  c("  Ben Franklin and Jefferson Davis","\tMillard Fillmore")
Renv$name.rex = "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"

FLenv = as.FL(Renv)


test_that("Check for regexpr function with pattern of string type for text2 ",{
    result = eval_expect_equal({
        test2 = regexpr("foo", text2)
    },Renv,FLenv,check.attributes = FALSE)
})


# ## Regular expressions not supported
# ## https://app.asana.com/0/143316600934101/144952239565760

test_that("Check for regexpr function with pattern of regular expression type for text1",{
          result = eval_expect_equal({
                   test1 = gregexpr("[a-z]", text1)
                },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })

# #Test failed for test3.
test_that("Check for regexpr function with pattern of both type (string and regular expression) for text3 ",{
          result = eval_expect_equal({
                   test3 = gregexpr("[gu]", text3)
                   test4 = gregexpr("en", text3)
                },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })

# #Test failed( But no error was shown and results were different)
test_that("Check for regexpr function with pattern of regular expression type",{
          result = eval_expect_equal({
                   test5 = gregexpr(name.rex,text4,perl = TRUE)[[2]]
                },Renv,FLenv,check.attributes = FALSE)
          print(result)
    })

Renv <- new.env(parent=globalenv())
Renv$txt <- c("arm","foot","lefroo", "bafoobar")

Renv$txt2 <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")

FLenv <- as.FL(Renv)


test_that("String functions",{
          eval_expect_equal({
            #txt <- c("arm","foot","lefroo", "bafoobar")
            if(length(i <- grep("foo", txt)))
              
              #txt converted to character to print in cat()
              cat("'foo' appears at least once in\n\t", as.character(txt), "\n")  
            
          },
          Renv,FLenv)
})
