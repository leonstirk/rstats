#! /usr/bin/env Rscript

# Combine all the motherfucking csv files
df <- do.call(rbind.fill, lapply(list.files(path = "./csv/", full.names = TRUE), read.csv))

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
write.csv(x = df, file = "dccRates.csv")

# Create a motherfucking dataframe for geocoding and save that motherfucker to a csv file for export.
geo <- df[,c("ID", "PropertyDetails.PropertyAddress", "PropertyDetails.PostalAddressForThisAssessment")]
write.csv(x = geo, file = "dccGeoSrc.csv")