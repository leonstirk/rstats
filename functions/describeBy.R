 describeBy <- function(x, groupName, groupVal, dp, conf) {
   ConfName <- paste(round(conf*100, digits = 0), "%", sep = '');
   LBName <- paste(ConfName, "confidence interval lower bound", sep = " ");
   UBName <- paste(ConfName, "confidence interval upper bound", sep = " ");
   
   xf <- x[groupName==groupVal];
   n <- length(xf);
   Mean <- mean(xf);
   SEM <- sqrt(var(xf)/length(xf));
   Var <- var(xf);
   SD <- stdev(xf);
   
   halfAlpha <- (1-conf)/2;
   MOE <- qnorm(1-halfAlpha)*SD/sqrt(n);
   
   UB <- Mean + MOE;
   LB <- Mean - MOE;
   
   TrimMean <- mean(xf, trim = 0.05);
   
   Median <- median(xf);
   Min <- min(xf);
   Max <- max(xf);
   Range <- Max - Min;
   IQRange <- IQR(xf);
   Skew <- skewness(xf);
   SES <- sqrt((6*n*(n-1))/((n-2)*(n+1)*(n+3)));
   Kurtosis <- kurtosis(xf);
   SEK <- 2*SES*sqrt( ((n^2)-1) / ((n-3)*(n+5)) );
   
   Names <- c("Mean", LBName, UBName, "5% trimmed mean", "Median", "Variance", "Standard deviation", "Minimum", "Maximum", "Range", "Interquartile range", "Skew", "Kurtosis");
   Stats <- c(Mean, LB, UB, TrimMean, Median, Var, SD, Min, Max, Range, IQRange, Skew, Kurtosis);
   SE <- c(SEM,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,SES,SEK);
   
   StatsFormatted <- formatStat(Stats, 2);
   SEFormatted <- formatStat(SE, 2);
 
   descriptivesBy <- data.frame(Names,StatsFormatted, SEFormatted);
   typeof(Names);
 }
 
 describeGroups <- function(x, factorList) {
   
 }
 
 formatStat <- function(x, dp) {
   format(round(x, dp), nsmall = dp)
 }
 