rm(list = ls())
library("latex2exp")

ice <- read.csv("ice_cream.csv")
xp <- 5:30 
x <- ice$temp 
y <- ice$yen

for (i.f in 1:3)
{
  # i.f <- 3
  if (i.f == 2) fit <- lm(yen ~ temp, data = ice)
  if (i.f == 3) fit <- lm(yen ~ temp+I(temp^2), data = ice)
  if (i.f > 1)
  {
    summary(fit)
    
    pred <- predict(fit, newdata = data.frame(temp = xp), interval = "prediction")
    yhat <- pred[, 1]
    lwr  <- pred[, 2]
    upr  <- pred[, 3]
  }

  #cairo_pdf("ice_cream.pdf", width = 7, height = 7, family = "UD Digi Kyokasho NP-R")
  svg(paste0("ice_cream", i.f, ".svg"), width = 6, height = 6, family = "UD Digi Kyokasho NP-R")
  
  matplot(x, y, type = "n",
          main = "Ice Cream Sales",
          xlab = "Temperature [deg. C]", 
          ylab = "Sales [Yen]")
  
  abline(v = seq(5, 30, 5), h = seq(400, 1400, 200), col = gray(0.8), lty = 2)
  
  # R RGB Color Codes Chart: https://www.rapidtables.com/web/color/RGB_Color.html
  matpoints(x, y, col = rgb(0.0, 0.0, 1.0, 0.4), pch = 16)
  
  if (i.f > 1)
  {
    matlines(xp, y = pred, col = "red", lwd = 2, lty = c(1, 2, 2))
   
    polygon(c(xp, rev(xp)), c(lwr, rev(upr)), border = F, col = rgb(0.8, 0.8, 0.8, 0.2)) 
    
    legend("topleft", col = "red", lty = 1:2,
           legend = c(TeX("$\\hat{y}$"), TeX("95\\% Prediction Interval")))
  }
  
  dev.off()
}

ice <- read.csv("ice_cream.csv")

fit <- lm(yen ~ temp, data = ice)

summary(fit)


















