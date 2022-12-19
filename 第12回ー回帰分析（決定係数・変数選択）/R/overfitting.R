rm(list = ls())
options(digits = 2)
library(latex2exp)
getwd()

x <- seq(0.1, 1, 0.1)
x <- seq(0.01, 1, 0.01) # Correct R^2 and AIC
(n <- length(x))
y.true <- sin(1 + x) 

set.seed(2)
e <- rnorm(n, mean = 0, sd = 0.03)
y.obs <- y.true + e
d <- data.frame(x, y = y.obs)

#set.seed(3)
#e2 <- rnorm(n, mean = 0, sd = 0.05)
#y.obs2 <- y.true + e2

for (p in 0:9)
{
#p <- 9

if (p == 0)
{
  fit <- lm(y~1, data = d)

} else
{
  fit <- lm(y~poly(x, p, raw = T), data = d)
}

sm <- summary(fit)
str(sm)
yfit <- fit$fitted

#png(paste0("overfitting_", p+1, ".png"))
#cairo_pdf(paste0("overfitting_", p+1, ".pdf"))
cairo_pdf(paste0("overfitting_", p+1, "n100.pdf"))

par(mar = c(2, 2, 3, 1) + 0.1)

matplot(x, y.obs, ylim = c(0.8, 1.1), 
main = TeX(paste("多項式回帰（$p=", p, "$）モデルを用いたフィッティング")),
        type = "n", xlab = "", ylab = "")
matlines(x, y.true, col = 1, lwd = 4)
matlines(x, yfit,   col = 4, lwd = 4)
matpoints(x, y.obs,  pch = 1)
#matpoints(x, y.obs2, pch = 16)

text(1.0, 0.88, adj = 1, cex = 1.5,
     TeX(paste("標本サイズ：$n=", n, "$")))
text(1.0, 0.86, adj = 1, cex = 1.5,
     TeX(paste("回帰係数の数：$p+1=", p+1, "$")))
text(1.0, 0.84, adj = 1, cex = 1.5,
     TeX(paste("決定係数：$R^2=", sm$r.squared, "$")))
text(1.0, 0.82, adj = 1, cex = 1.5,
     TeX(paste("自由度調整済み決定係数：$Adj.R^2=", sm$adj.r.squared, "$")))
text(1.0, 0.80, adj = 1, cex = 1.5,
     TeX(paste("AIC：$", AIC(fit), "$")))

legend("topleft", bty = "n",
       legend = c(TeX("学習用観測値： $y=\\sin(1+x)+\\epsilon, \\epsilon\\sim \\textit{N}(0,0.05^2)$"),
                  TeX("真のモデル： $y=\\sin(1+x)$"), 
                  TeX(paste0("多項式回帰モデル：$y = b_0+\\cdots+b_{", p, "}x^{", p, "}$"))), 
       pch = c(1, NA, NA),
       lty = c(NA, 1, 1),
       lwd = c(NA, 4, 4),
       col = c(1, 1, 4))
dev.off()
}

