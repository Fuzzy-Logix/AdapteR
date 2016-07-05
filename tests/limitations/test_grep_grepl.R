Renv <- new.env(parent=globalenv())
Renv$l <- letters
Renv$txt2 <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")

FLenv <- as.FL(Renv)


## regular expressions not supported
## https://app.asana.com/0/136555696724848/144952239565760
test_that("String functions",{
    eval_expect_equal({
        test1 <- grep("[a-z]", l)},
        Renv,FLenv)
})

test_that("String functions",{
    eval_expect_equal({
        ( i <- grep("[gu]", txt2) ) # indices
        stopifnot( txt2[i] == grep("[gu]", txt2, value = TRUE) )
    },
    Renv,FLenv)
})
