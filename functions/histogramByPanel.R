histByPanel <- function(x, factorGroup){
  par(mfrow=c(1,4))
  
  lv <- levels(factorGroup)
  
  for(level in lv) {
    g <- x[factorGroup==level]
    h <- hist(g, col="gray", main = level, xlab = "", labels = FALSE, ylim = c(0,120), xlim = c(0,140), breaks = 6)
    xfit <- seq(min(0), max(g)) # length = 40
    yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
    yfit <- yfit * diff(h$mids[1:2]) * length(g) 
    lines(xfit, yfit, col = "black", lwd = 2)
  }
}