rm(list = ls())
library(latex2exp)

#
# Logit and Sigmoid Functions
#
logit <- function(p) log(p/(1-p))
sigmoid <- function(y) 1/(1+exp(-y))

logit(p = 0.0)
logit(p = 0.2)
logit(p = 0.5)
logit(p = 0.8)
logit(p = 1.0)

p <- seq(0, 1, 0.001)
y <- logit(p)

cairo_pdf("logit_transform_graph.pdf")
par(cex = 1.5)
matplot(x = p, y = y, type = "l", lwd = 2, col = 2,
main = TeX("$y=logit(p)=\\ln \\frac{p}{1-p}$"))
grid()
dev.off()

cairo_pdf("sigmoid_graph.pdf")
par(cex = 1.5)
matplot(x = y, y = p, type = "l", lwd = 2, col = 2,
main = TeX("$y=logit(p)=\\ln \\frac{p}{1-p}$"))
grid()
dev.off()

#
# University exam result
#
y <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
x <- c(9, 8, 7, 6, 5, 4, 5.2, 1, 8.2, 7.2, 3, 4, 1, 1.2, 2.2, 7, 2, 3.2, 0.2, 0)
d <- data.frame(x, y)
dtex <- data.frame(勉強時間 = x, 合格 = y)
library(xtable)
write.csv(dtex, file = "univ_exam_data.csv", row.names = F, quote = F)
print(xtable(dtex), file = "univ_exam_data.tex")

cairo_pdf("apply_reg_to_binary.pdf")
par(cex = 1.2)
matplot(x = x, y = y, pch = 1,
        ylim = c(0, 1.2), xlim = c(0, 12),
        main = "２値データに対して回帰モデルを適合させた失敗図",
        xlab = "平均勉強時間 / 日",
        ylab = "合格(1)，不合格(0)")
grid()
fit <- lm(y ~ x, data = d)
summary(fit)
abline(a = coef(fit)[1], b = coef(fit)[2], col = 2)
text(x = 4, y = 0.4, adj = 0, TeX("$\\leftarrow p=b_0 + b_1 x$"))
legend("topleft", pch = c(1, NA), lty = c(NA, 1), col = c(1, 2),
       legend = c("合格(1)，不合格(0)", "単回帰モデル"))
dev.off()

cairo_pdf("apply_reg_to_logit.pdf")
yl <- logit(y)
yl[yl == Inf] <- 100
yl[yl == -Inf] <- -100
dl <- data.frame(x, yl)
par(cex = 1.2)
matplot(x = x, y = yl, pch = 1, yaxt = "n",
        ylim = c(-100, 100),
        main = "ロジット変換したデータに回帰モデルを適合させた概念図",
        xlab = "平均勉強時間 / 日",
        ylab = "合格(∞)，不合格(-∞)")
grid()
fit <- lm(yl ~ x, data = dl)
summary(fit)
modelline <- function(x) coef(fit)[1] + coef(fit)[2] * x
xm <- seq(0, 8.5, 0.1)
matlines(x = xm, y = modelline(xm), type = "l", col = 2)

text(x = 4.5, y = 0.4, adj = 0, 
     TeX("$\\leftarrow \\ln \\left(\\frac{p}{1-p}\\right)=b_0 + b_1 x$"))
legend("topleft", pch = c(1, NA), lty = c(NA, 1), col = c(1, 2),
       legend = c("合格(∞)，不合格(-∞)", "単回帰モデル"))
mtext(side = 2, adj = 0.02, cex = 2.0, "-∞")
mtext(side = 2, adj = 0.98, cex = 2.0, "∞")
dev.off()

cairo_pdf("inverse_logit.pdf")
par(cex = 1.2)
matplot(x = x, y = y, pch = 1,
        ylim = c(0, 1.2), xlim = c(0, 10),
        main = "ロジット逆変換（ロジスティック変換）して元に戻した場合",
        xlab = "平均勉強時間 / 日",
        ylab = "合格(1)，不合格(0)")
grid()

fit <- glm(y ~ x, data = d, family = "binomial")
summary(fit)
xp <- seq(0, 10, 0.01)
logodds <- predict(fit, newdata = data.frame(x = xp))
matlines(x = xp, y = sigmoid(logodds), lty = 1, col = 2, lwd = 2)

text(x = 3.8, y = 0.4, adj = 0, 
     TeX("$\\leftarrow p=\\frac{1}{1+e^{-(b_0 + b_1 x)}}$"))
legend("topleft", pch = c(1, NA), lty = c(NA, 1), col = c(1, 2),
       legend = c("合格(1)，不合格(0)", "単回帰モデル"))
dev.off()

#
# Odds ratio(OR)
#
(OR <- exp(fit$coef[2]))

system("cd .. && touch lec.tex && make lecho")

#fit <- glm(y~x-1, data = d, family = "binomial")
#summary(fit)
#fit$fitted
#
#n <- length(y)
#p.hat <- sum(y)/n  
#p <- rep(NA, n)
#p[y == 1] <- 1.0 - 0.01
#p[y == 0] <- 0.0 + 0.01
#d <- data.frame(y = logit(p), x)
#fit <- lm(y~x, data = d)
#(yfit <- sigmoid(fit$fitted))
#summary(fit)
#
#matplot(x = x, y = yfit)
#dev.off()
#
#p <- c(0.8, 0.8, 0.2, 0.2)
#y <- logit(p)
#x <- c(1, 1, 0, 0)
#d <- data.frame(x, y)
#d
#fit <- lm(y~x, data = d)
#summary(fit)
#p <- sigmoid(fit$fitted)
#
