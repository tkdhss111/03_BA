rm(list = ls())
library(latex2exp)

set.seed(10)

x <- 1:10
y <- c(1, 4, 7, 2, 5, 8, 9, 6, 5, 11)
fit <- lm(y~x, data = data.frame(x, y))

draw.lsline <- function(a = 0, b = 1, is.png = T, is.leg = F, is.ss = T, main, file)
{
  # a <- 1; b <- 1; main = "main"; file = "test.png"
  yhat <- a + b*x 
  e <- y - yhat
  ss <- sum(e^2)

  if (is.png)
  {
    #png(file, width = 5, height = 5, family = "UD Digi Kyokasho NP-R")
    #png(file, width = 480, height = 480, family = "UD Digi Kyokasho NP-R")
    png(file, width = 480, height = 480)
  }
  
  matplot(x, y, type = "n", 
          cex.lab = 2.0,
          cex.axis = 2.0,
          cex.main = 2.0,
          xlim = c(1, 11),
          ylim = c(1, 11),
          main = main,
          xlab = "x",
          ylab = "y")
  
  abline(h = seq(-30, 30, 1),
         v = seq(-30, 30, 1),
         col = gray(0.9), lty = 2)
  
  abline(a = a, b = b, lwd = 2, col = rgb(1, 0, 0, 0.25))
  
  matpoints(x, y, pch = 16, cex = 1, col = "blue")
 
  if (is.ss)
  {
    matpoints(x, yhat, pch = 1,  cex = 2, col = "black")
    segments(x, y, x, yhat, col = "blue")
    text(2.5, 11.1, TeX("$\\sum e^2=$"), pos = 1, cex = 3.0)
    text(4.0, 10.2, round(ss), pos = 4, cex = 3.0)
  }
 
  if (is.leg) legend("bottomright", pch = c(16, 1), col = c("blue", "black"),
                     legend = c(TeX("$y_i$"), TeX("$\\hat{y}_i$")))
  
  if (is.png) for (i.g in dev.list()) dev.off(i.g)
}

draw.lsline(a =  2, b =  1/3, is.ss = F, main = "Line No.1", file = "line01.png")
draw.lsline(a = -1, b =  2/2, is.ss = F, main = "Line No.2", file = "line02.png")
draw.lsline(a =  1, b =  1/2, is.ss = F, main = "Line No.3", file = "line03.png")
draw.lsline(a =  0, b =  2/2, is.ss = F, main = "Line No.4", file = "line04.png")
draw.lsline(a = 10, b = -2/2, is.ss = F, main = "Line No.5", file = "line05.png")
draw.lsline(a =  6, b =    0, is.ss = F, main = "Line No.6", file = "line06.png")

draw.lsline(a =  2, b =  1/3, is.ss = T, main = "Line No.1", file = "line_ss01.png")
draw.lsline(a = -1, b =  2/2, is.ss = T, main = "Line No.2", file = "line_ss02.png")
draw.lsline(a =  1, b =  1/2, is.ss = T, main = "Line No.3", file = "line_ss03.png")
draw.lsline(a =  0, b =  2/2, is.ss = T, main = "Line No.4", file = "line_ss04.png")
draw.lsline(a = 10, b = -2/2, is.ss = T, main = "Line No.5", file = "line_ss05.png")
draw.lsline(a =  6, b =    0, is.ss = T, main = "Line No.6", file = "line_ss06.png")

c <- coef(fit)
draw.lsline(a =  c[1], b = c[2], is.png = T, is.ss = T, is.leg = T, 
            file = "ls_line.png", main = "Least Square Line")
            #main = TeX("$\\hat{y}_i=\\hat{\\beta}_0 + \\hat{\\beta}_1 x_i$"))
