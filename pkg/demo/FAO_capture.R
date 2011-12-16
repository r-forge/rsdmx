library(RSDMX)

CAPTURE_KEYFAMILY  <-read.sdmx(url="http://www.fao.org/figis/sdmx/registry/KeyFamily/CAPTURE_KEYFAMILY/FAO/0.1")
summary(CAPTURE_KEYFAMILY)

CAPTURE_DSD  <-read.sdmx(url="http://www.fao.org/figis/sdmx/registry/DataStructure/CAPTURE_DSD/FAO/0.1")
summary(CAPTURE_DSD)

cd<-getCodeList(CAPTURE_DSD)
cd_area <- getCodeList(CAPTURE_DSD, listName="CL_PRODUCTION_AREA")
head(cd_area )
