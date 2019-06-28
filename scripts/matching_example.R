rm(list = ls())

n <- 1000
mean_1 <- 135
sd_1 <- 55
mean_2 <- 242
sd_2 <- 143

makeDist <- function(n, mean_1, sd_1, mean_2, sd_2) {
  floor <- rnorm(n = n, mean = mean_1, sd = sd_1)
  price <- rnorm(n = n, mean = mean_2, sd = sd_2)
  control_f <- data.frame(cbind(floor, price, rep(0,n)))

  floor <- rnorm(n = n, mean = mean_1+(mean_1/2), sd = sd_1)
  price <- rnorm(n = n, mean = mean_2+(mean_2/2), sd = sd_2)
  control_r <- data.frame(cbind(floor, price, rep(1,n)))

  floor <- rnorm(n = n, mean = mean_1-(mean_1/2), sd = sd_1)
  price <- rnorm(n = n, mean = mean_2-(mean_2/2), sd = sd_2)
  treatment <- data.frame(cbind(floor, price, rep(2,n)))

  return(rbind(control_f, control_r, treatment))
}

result <- makeDist(n, mean_1, sd_1, mean_2, sd_2)

names(result)[3] <- 'group'
result$group <- as.factor(result$group)

plot <- ggplot(result[which(result$group %in% c(1,2,3)),], aes(floor, price, color = group)) + geom_point()