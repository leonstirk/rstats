#! /usr/bin/env Rscript

library('plyr')

i <- 31687
while ( i < 31688 ) {

hello <- sprintf("/home/ubuntu/rstats/%s/", i)

# Combine all the motherfucking csv files
df <- do.call(rbind.fill, lapply(list.files(path = hello, full.names = TRUE), read.csv))

# Attach a motherfucking ExtractionDate variable that's set to whatever date this script is run
a <- rep(as.Date(Sys.Date()), nrow(df))
df <- cbind(df,a)
names(df)[names(df) == 'a'] <- 'ExtractionDate'

# Bit of a motherfucking cleanup. Replace all instnaces of 'null' with NA
df[apply(df, 2, function(x) x=="null")] = NA

# Change some of these whack motherfucking variable names
names(df)[names(df) == 'PropertyDetails.Certificate.s.OfTitle'] <- 'PropertyDetails.CertificatesOfTitle'
names(df)[names(df) == 'PropertyDetails.RatepayerName.s.'] <- 'PropertyDetails.RatepayerNames'
#names(df)[names(df) == 'old'] <- 'new'

# Write the motherfucking dataframe to a csv file for motherfucking posterity motherfucker! Boom!
write.csv(x = df, file = sprintf("dccRates%s.csv", i))

# Create a motherfucking dataframe for geocoding and save that motherfucker to a csv file for export.
geo <- df[,c("ID", "PropertyDetails.PropertyAddress", "PropertyDetails.PostalAddressForThisAssessment")]
write.csv(x = geo, file = sprintf("dccGeoSrc%s.csv", i))

i=i+1

}