rm(list = ls())
setwd("C:/Users/hss/0_tkd/1_hss/TIU/01_ds/03_BA/data/auto-mpg")
getwd()
d0 <- read.table("auto-mpg-clean.data")
d0
colnames(d0) <- c("燃費", 
                  "気筒数",
                  "排気量",
                  "馬力",     # HP
                  "車体重量", # Lb -> kg
                  "加速性能", # 時速97km(60mile)に到達する秒数
                  "製造年",
                  "分類",
                  "車名")
d1 <- d0
d1[, "燃費"]     <- round(d0[, "燃費"] * 0.425144, 2)  # MPG -> km/L
d1[, "排気量"]   <- round(d0[, "排気量"] * 16.387)     # cu.in. -> cc
d1[, "車体重量"] <- round(d0[, "車体重量"] * 0.453592) # Lb -> kg
d1[, "製造年"]   <- 1900 + d0[, "製造年"] # 2 digits -> 4 digits
d1[d1[, "分類"] == 1, "分類"] <- "アメ車"
d1[d1[, "分類"] == 2, "分類"] <- "欧州車"
d1[d1[, "分類"] == 3, "分類"] <- "日本車"

d1$経年 <- d1[, "製造年"] - max(d1[, "製造年"])

head(d1)

d2 <- d1[, c("燃費", "気筒数", "排気量", "馬力", "車体重量", "加速性能", "経年")]
library(psych)
#d2$燃費 <- log(1/d2$燃費)
d2$排気量 <- 1/d2$排気量
pairs.panels(d2)

n <- nrow(d1)
ii.train <- sample(1:n, floor(n/5))
d.train <- d1[ii.train, ]
d.test <- d1[-ii.train, ]

fit0 <- lm(燃費 ~ 気筒数 + 排気量 + 馬力 + 車体重量 + 加速性能 + 分類 + 経年, data = d.train)
fit <- lm(I(1/燃費) ~ 気筒数 + 排気量 + 馬力 + 車体重量 + 加速性能 + 分類 + 経年, data = d.train)
fit1 <- lm(log(燃費) ~ 気筒数 + 排気量 + 馬力 + 車体重量 + 加速性能 + 分類 + 経年, data = d.train)
fit2 <- lm(燃費 ~ 気筒数 + 排気量 + 馬力 + 車体重量 + 分類 + 経年, data = d.train)
fit3 <- lm(燃費 ~ (気筒数 + 排気量 + 馬力 + 車体重量 + 分類 + 経年)^2, data = d.train)
fit4 <- lm(log(燃費) ~ (気筒数 + 排気量 + 馬力 + 車体重量 + 分類 + 経年)^2, data = d.train)

summary(fit0)
summary(fit)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
#plot(fit1)
#plot(fit2)

library(MASS)
stepAIC(fit2)
stepAIC(fit3)
fitstep4 <- stepAIC(fit4)
summary(fitstep4)

bc <- boxcox(fit)

y <- d.test$燃費
yhat <- 1/predict(fit, newdata = d.test)
yhat1 <- exp(predict(fit1, newdata = d.test))
yhat1 <- predict(fit1, newdata = d.test)
yhat3 <- predict(fit3, newdata = d.test)
yhat4 <- exp(predict(fitstep4, newdata = d.test))
e <- yhat1 - y
e
get.MAPE <- function(y, yhat) mean(abs(yhat - y)/y * 100)

(MAPE <- get.MAPE(y, yhat))
(MAPE1 <- get.MAPE(y, yhat1))
(MAPE3 <- get.MAPE(y, yhat3))
(MAPE4 <- get.MAPE(y, yhat4))


