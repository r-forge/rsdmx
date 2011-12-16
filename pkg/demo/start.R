library(RSDMX)

# DATASET example:
aqua.min <- system.file("exampleData", "AQUACULTURE_minimal2.xml", package = "RSDMX")


# KEY FAMILY example:
keypath <- system.file("exampleData", "aqua_keyfa.xml", package = "RSDMX")
keyfaTP<-read.sdmx(keypath)

getDimOld(keyfaTP)
getTimeDim(keyfaTP)
getKeyFamily(keyfaTP)
getAttr(keyfaTP)
getPriMes(keyfaTP)

# CODELIST example:
sdmx<-system.file("exampleData", "major_areas.xml", package= "RSDMX")
sdmxTP<-xmlTreeParse(sdmx, useInternalNodes = TRUE)

getCodeList(sdmxTP,"CL_FAO_MAJOR_AREA")[1:2,]

##choose language:
getCodeList(sdmxTP,"CL_FAO_MAJOR_AREA", lang="fr")[1:2,,drop=FALSE]

## SERIE example1
serie<-system.file("exampleData", "capture.xml", package= "RSDMX")
serieTP<-xmlTreeParse(serie, useInternalNodes = TRUE)
df<-getSerie(serieTP)
df[1:20,]

