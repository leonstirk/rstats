#!/usr/bin/env Rscript


# Create a motherfucking dataframe for geocoding and save that motherfucker to a csv file for export.
geo <- df[,c("ID", "PropertyDetails.PropertyAddress", "PropertyDetails.PostalAddressForThisAssessment")]
write.csv(x = geo, file = "dccGeoSrc.csv")

