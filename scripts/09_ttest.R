# Reproducing output from t.test
# reference: https://en.wikipedia.org/wiki/Student%27s_t-test


# load derived data
load("data/derived/marathon.RData")


# unequal variance ----

# mean, sd, and n for female/male
mu_f <- mean(marathon$na[marathon$sex == "Female"])
sd_f <- sd(marathon$na[marathon$sex == "Female"])
n_f <- sum(marathon$sex == "Female")
mu_m <- mean(marathon$na[marathon$sex == "Male"])
sd_m <- sd(marathon$na[marathon$sex == "Male"])
n_m <- sum(marathon$sex == "Male")

# test statistic
t1 <- (mu_m - mu_f)/(sqrt(sd_f^2/n_f + sd_m^2/n_m))
t1
# df (degrees of freedom)
v <- (sd_f^2/n_f + sd_m^2/n_m)^2/(sd_f^4/(n_f^2*(n_f - 1)) + sd_m^4/(n_m^2*(n_m - 1)))
v
# p-value
pv1 <- 2*pt(t1, df = v, lower.tail = FALSE)
pv1

# reproduced output
list("test" = round(t1, 4), "df" = round(v, 1), "p-value" = round(pv1, 7))
# compare with
t.test(na ~ sex, data = marathon, var.equal = FALSE)


# equal variance ----

# pooled standard deviation
s_p <- sqrt(((n_m - 1)*sd_m^2 + (n_f - 1)*sd_f^2)/(n_m + n_f - 2))
# test statistic
t2 <- (mu_m - mu_f)/(s_p*sqrt(1/n_m + 1/n_f))
# df (degrees of freedom)
n_m + n_f - 2
# p-value
pv2 <- 2*pt(t2, df = n_m + n_f - 2, lower.tail = FALSE)

# reproduced output
list("test" = round(t2, 4), "df" = n_m + n_f - 2, "p-value" = round(pv2, 8))
# compare with
t.test(na ~ sex, data = marathon, var.equal = TRUE)
