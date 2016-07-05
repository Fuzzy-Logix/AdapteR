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
          ##print(result)
    })
