rm(list = ls())

n <- 100

b0 <- 8.0
b1 <- 4.0
b2 <- 2.0

set.seed(1)
e <- rnorm(n, mean = 0, sd = 10)

get.summary <- function(d, id)
{
  # id <- 1
  file <- paste0("summary_lm", id, ".tex")
  cat(file, fill = T)

  fit <- lm(y~., data = d)

  sm <- summary(fit)

  capture.output(sm, file = file)

  system(paste("sed -i '1,8d'", file))
}

#
# y is noise model
#
id <- 1
x1 <- 1:n
x2 <- x1 ^ 2
y  <- e
d  <- data.frame(y, x1, x2)
get.summary(d, id)

#
# y is constant(intercept only) model
#
id <- 2
y  <- b0 + e
d  <- data.frame(y, x1, x2)
get.summary(d, id)

#
# y is model with intercept and x1
#
id <- 3
y  <- b0 + b1 * x1 + e
d  <- data.frame(y, x1, x2)
get.summary(d, id)

#
# y is model with intercept, x1 and x2
#
id <- 4
y  <- b0 + b1 * x1 + b2 * x2 + e
d  <- data.frame(y, x1, x2)
get.summary(d, id)

#
# Matrix manupulation
#
X <- matrix(c(rep(1, n), x1, x2), nrow = n)
X

b <- solve(t(X) %*% X) %*% t(X) %*% y
b

fit <- lm(y~., data = d)
summary(fit)

#library(texreg)
#
#texreg(fit,
#       #custom.model.names = c("M1"),
#       fontsize = "footnotesize", 
#       booktabs = T, dcolumn = T, single.row = T, use.packages = F, 
#       file = "texreg.tex")
#
#system("sed -i /caption/d texreg.tex") # Delete caption

