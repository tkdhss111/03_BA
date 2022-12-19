rm(list = ls())
#options(encoding = "utf-8")
library(latex2exp)

set.seed(10)

draw.scatter <- function(f, file, main)
{
  x <- rnorm(200, mean = 0, sd = 10)
  x.s <- -30:30
  e <- rnorm(length(x), mean = 0, sd = 5)
  y.true <- f(x)
  y <- y.true + e

  #fit <- lm(y ~ x, data = data.frame(x, y))
  #yhat <- fit$fitted

  cairo_pdf(file, width = 5, height = 5,
            family = "UD Digi Kyokasho NP-R")

  par(mar = c(4, 4, 2, 1) + 0.1)

  matplot(x, y,
          type = "n", 
          main = main,
          xlab = "Variable 1",
          ylab = "Variable 2",
          xlim = c(-20, 20),
          ylim = c(-20, 20))

  abline(h = seq(-30, 30, 10),
         v = seq(-30, 30, 5),
         col = gray(0.8), lty = 2)

  abline(h = 0, v = 0, col = gray(0.2))

  matpoints(x, y, pch = 16, col = rgb(0, 0, 1, 0.25))
 
  # True curve 
  matlines(x.s, f(x.s), col = "red")

  for (i.g in dev.list()) dev.off(i.g)
}

x <- 1:10
y <- 1:10

f <- function(x) x
draw.scatter(f, "positive_correlation.pdf", "Positive Correlation")

f <- function(x) -x
draw.scatter(f, "negative_correlation.pdf", "Negative Correlation")

f <- function(x) 0.1*x^2 - 10
draw.scatter(f, "curvilinear_correlation.pdf", "Curvilinear Correlation")

f <- function(x) x^0
draw.scatter(f, "no_correlation.pdf", "No Correlation")

#
# Pitfall Example of Correlations
#
x <- rnorm(200, mean = 0, sd = 15)
x.s <- -30:30
e <- rnorm(length(x), mean = 0, sd = 5)
y.true <- f(x)
y <- y.true + e

r <- cor(x, y, method = "pearson")
rho <- cor(x, y, method = "spearman")

cairo_pdf("pitfall_correlation.pdf", width = 5, height = 5,
          family = "UD Digi Kyokasho NP-R")

par(mar = c(4, 4, 2, 1) + 0.1)

matplot(x, y,
        type = "n", 
        main = main,
        xlab = "Variable 1",
        ylab = "Variable 2",
        xlim = c(-20, 20),
        ylim = c(-20, 20))

abline(h = seq(-30, 30, 10),
       v = seq(-30, 30, 5),
       col = gray(0.8), lty = 2)

abline(h = 0, v = 0, col = gray(0.2))

matpoints(x, y, pch = 16, col = rgb(0, 0, 1, 0.25))

# True curve 
matlines(x.s, f(x.s), col = "red")

legend("bottomleft",
       legend = c(
       TeX(sprintf("Pearson $R = %2.1f$", r)),
       TeX(sprintf("Spearman $\\rho = %2.1f$", r))))

for (i.g in dev.list()) dev.off(i.g)
