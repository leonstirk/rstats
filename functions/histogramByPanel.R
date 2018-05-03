library(FSA)

histByPanel <- function(x, factorGroup, binWidth){
  par(mfrow=c(4,4))
  
  max <-  max(x)
  lv <- levels(factorGroup)
  
  for(level in lv) {
    g <- x[factorGroup==level]
    h <- hist(g, col="gray", main = level, xlab = "", labels = FALSE, xlim = c(0,max), w=binWidth)
    # xfit <- seq(min(0), max(g)) # length = 40
    # yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
    # yfit <- yfit * diff(h$mids[1:2]) * length(g) 
    # lines(xfit, yfit, col = "black", lwd = 2)
  }
}