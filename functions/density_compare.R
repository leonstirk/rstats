require(sm)

round100 <- function(x) { 100*ceiling(quantile(x, .99))/100 }

densityCompare <- function(x, y, lab, title) {
  sm.density.compare(x, y, model = "equal", xlab=lab, xlim = c(0,round100(x)))
  title(main = title)
}