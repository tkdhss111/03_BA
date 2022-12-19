rm(list = ls())
options(digits = 2)

VIF <- seq(1, 15, 0.01)

get.R <- function(VIF)
{
  sqrt(1 - 1 / VIF)
}


R <- get.R(VIF)
R

png("vif_r.png")
par(cex = 1.5)
matplot(x = VIF, y = R, type = "l", 
        main = "分散拡大係数VIFと相関係数Rの関係")
abline(v = 10, col = "red")
grid()
dev.off()

head(mtcars)

fit <- lm(mpg ~ ., data = mtcars)
summary(fit)
step(fit)

#
# VIF (variance inflation factor)
#
head(mtcars)
(fit <- lm(mpg ~ ., data = mtcars))

library(car)
vif(fit)
