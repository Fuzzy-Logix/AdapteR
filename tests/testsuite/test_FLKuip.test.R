# kuiper Test.
str <- paste0("SELECT * FROM tblKuiperTest")
res <- sqlQuery(connection, str)
alpha <- as.FL(res1[res1$groupid == 1, ]$num_val)
beta <- as.FL(res1[res1$groupid == 2, ]$num_val)
kuip.test(alpha, beta)
