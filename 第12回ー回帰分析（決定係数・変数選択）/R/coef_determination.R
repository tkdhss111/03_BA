rm(list = ls())

x <- 1:30

y <- rep(5, 30) + rnorm(30)

d <- data.frame(x, y)
fit <- lm(y~., data = d)
summary(fit)
yhat <- fit$fitted

yhat <- rep(8, 30)


ybar <- mean(y)

SST <- sum((y - ybar)^2)
SSR <- sum((yhat - ybar)^2)
SSE <- sum((y - yhat)^2)

(R2 <- SSR/SST)
(R2 <- 1-SSE/SST)

library(latex2exp)
png("negative_coef_determination.png")
matplot(x, y, pch = 16, ylim = c(0, 10),
        main = TeX(paste("決定係数 $R^2=$", round(R2,2))))
matlines(x, yhat, col = 2)
legend("topright", 
       legend = c("数理モデル", "観測値"), 
       col = c(2, 1),
       pch = c(NA, 16),
       lty = c(1, NA))
dev.off()

