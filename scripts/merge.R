#!/usr/bin/env Rscript

library('plyr')

# Combine all the motherfucking csv files
df <- do.call(rbind.fill, lapply(list.files(path = "/home/ubuntu/rstats/csv/", full.names = TRUE), read.csv))


# Attach a motherfucking ExtractionDate variable that's set to whatever date this script is run
a <- rep(as.Date(Sys.Date()), nrow(df))
df <- cbind(df,a)
names(df)[names(df) == 'a'] <- 'ExtractionDate'


# Bit of a motherfucking cleanup. Replace all instnaces of 'null' with NA
df[apply(df, 2, function(x) x=="null")] = NA
df[apply(df, 2, function(x) x==" null")] = NA
df[apply(df, 2, function(x) x=="null ")] = NA


# Change some of these whack motherfucking variable names
# Syntax: names(df)[names(df) == 'old'] <- 'new'
names(df)[names(df) == 'PropertyDetails.Certificate.s.OfTitle'] <- 'PropertyDetails.CertificatesOfTitle'
names(df)[names(df) == 'PropertyDetails.RatepayerName.s.'] <- 'PropertyDetails.RatepayerNames'

names(df)[names(df) == 'RatesBreakdown.NETTOTALGENERALRATES.GrossGeneralRate'] <- 'RatesBreakdown.NetTotalGeneralRates.GrossGeneralRate'
names(df)[names(df) == 'RatesBreakdown.NETTOTALGENERALRATES.NetGeneralRate'] <- 'RatesBreakdown.NetTotalGeneralRates.NetGeneralRate'
names(df)[names(df) == 'RatesBreakdown.TOTALRATES.GrossGeneralRate'] <- 'RatesBreakdown.TotalRates.GrossGeneralRate'
names(df)[names(df) == 'RatesBreakdown.TOTALRATES.NetGeneralRate'] <- 'RatesBreakdown.TotalRates.NetGeneralRate'


# Remove other whack variables motherfucker.
# Syntax: df = subset(df, select = -c(name,name))

df = subset(df, select = -c(RatesBreakdown.GeneralRates.GrossGeneralRate))
df = subset(df, select = -c(RatesBreakdown.GeneralRates.NetGeneralRate))
df = subset(df, select = -c(RatesBreakdown.LessDividend.InvestmentIncome.NetGeneralRate))
df = subset(df, select = -c(RatesBreakdown.CommunityServicesRates.GrossGeneralRate))
df = subset(df, select = -c(RatesBreakdown.CommunityServicesRates.NetGeneralRate))
df = subset(df, select = -c(RatesBreakdown.OtherTargetedRates.GrossGeneralRate))
df = subset(df, select = -c(RatesBreakdown.OtherTargetedRates.NetGeneralRate))

# Tidy up the filthy motherfucking names factor variables and parse them into lists.
# Motherfucker! You can access the list elements using df$var[[row]][[element]]
# i.e. df$PropertyDetails.RatepayerNames[[1]][[2]]
# Also count the number of owners with length(df$PropertyDetailsRatepayerNames[[1]])

df$PropertyDetails.RatepayerNames <- strsplit(as.character(df$PropertyDetails.RatepayerNames)," :")
df$PropertyDetails.RatepayerNames <- lapply(df$PropertyDetails.RatepayerNames, function(x) { trimws(x) })


# Write the motherfucking dataframe to a csv file for motherfucking posterity motherfucker! Boom!
save(df, file="dccRates1.Rda")


