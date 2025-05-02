install.packages("countrycode")  # if not already installed
library(countrycode)

setwd("/Users/sruthikp/Work/Analysis/TropicalEcologyLitReview/BiotropicaManuscriptFiles")

df <- read.csv("UNHDIValues.csv")

df$iso3 <- countrycode(df$Country, 
                       origin = "country.name", 
                       destination = "iso3c")

write.csv(df, "UNHDIValuesWithISO3.csv")

