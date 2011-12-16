library(RSDMX)

# FAOSTAT example:
## dsd
faostat.dsd.path <- system.file("exampleData", "ESS_DSDFAOSTAT.xml", package = "RSDMX")
faostat <- read.sdmx(faostat.dsd.path)
summary(faostat)

## data
faostat.data.path <- system.file("exampleData", "ESS_ExempleDataCountrySTAT.xml", package = "RSDMX")
faostat_data <- read.sdmx(faostat.data.path)
summary(faostat_data)

df<-as.data.frame(faostat_data)
df
sapply(df,class)
with(df,aggregate(obsValue, list(COMMODITY), mean))

