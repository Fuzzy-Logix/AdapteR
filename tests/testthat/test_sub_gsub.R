#ERROR https://app.asana.com/0/143316600934101/144952239565760
#gsub
Renv <- new.env(parent=globalenv())


Renv$txt <- c("The", "licenses", "for", "most", "software", "are",
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
    test1 <- gsub("([ab])", "\\1_\\1_", "abc and ABC")},Renv)})

test_that("String functions",{
  eval_expect_equal({
    
    (ot <- sub("[b-e]",".", txt))
    test1 <- txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution
    
    test2 = txt[gsub("g","#", txt) != gsub("g","#", txt, ignore.case = TRUE)] # the "G" words
  },Renv)})


test_that("String functions",{
  eval_expect_equal({
    
    ## trim trailing white space
    str <- "Now is the time      "
    test1 <- sub(" +$", "", str)  ## spaces only
    ## what is considered 'white space' depends on the locale.
    test2 <- sub("[[:space:]]+$", "", str) ## white space, POSIX-style
    ## what PCRE considered white space changed in version 8.34: see ?regex
    test3 <- sub("\\s+$", "", str, perl = TRUE) ## PCRE-style white space
  },Renv)})



test_that("String functions",{
  eval_expect_equal({
    ## capitalizing
    txt <- "a test of capitalizing"
    test1 <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt, perl=TRUE)
    test2 <- gsub("\\b(\\w)",    "\\U\\1",       txt, perl=TRUE)
  },Renv)})



test_that("String functions",{
  eval_expect_equal({
    txt2 <- "useRs may fly into JFK or laGuardia"
    test1 <- gsub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
    test2 <- sub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
  },Renv)})









