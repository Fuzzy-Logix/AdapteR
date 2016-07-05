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
Renv$str <- "Now is the time      "
Renv$txt1 <- "a test of capitalizing"
Renv$txt2 <- "useRs may fly into JFK or laGuardia"

FLenv <- as.FL(Renv)

test_that("sub and gsub with regular expressions",{
    eval_expect_equal({
        test1 <- gsub("([ab])", "\\1_\\1_", "abc and ABC")
        (ot <- sub("[b-e]",".", txt))
        test2 <- txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution
        
        test3 = txt[gsub("g","#", txt) != gsub("g","#", txt, ignore.case = TRUE)] # the "G" words
        ## trim trailing white space
        test4 <- sub(" +$", "", str)  ## spaces only
        ## what is considered 'white space' depends on the locale.
        test5 <- sub("[[:space:]]+$", "", str) ## white space, POSIX-style
        ## what PCRE considered white space changed in version 8.34: see ?regex
        test6 <- sub("\\s+$", "", str, perl = TRUE) ## PCRE-style white space
        ## capitalizing
        test7 <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt1, perl=TRUE)
        test8 <- gsub("\\b(\\w)",    "\\U\\1",       txt1, perl=TRUE)
        test9 <- gsub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
        test10 <- sub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
    },
    Renv,FLenv)
})









