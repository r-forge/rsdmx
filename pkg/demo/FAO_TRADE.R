library(RSDMX)

TRADE_KEYFAMILY  <-read.sdmx(url="http://www.fao.org/figis/sdmx/registry/KeyFamily/TRADE_KEYFAMILY/FAO/0.1")
summary(TRADE_KEYFAMILY)

TRADE_DSD  <-read.sdmx(url="http://www.fao.org/figis/sdmx/registry/DataStructure/TRADE_DSD/FAO/0.1")
summary(TRADE_DSD)

cd<-getCodeList(TRADE_DSD)
cd_UN_COUNTRY <- getCodeList(TRADE_DSD, listName="CL_UN_COUNTRY")
cd_UNIT <- getCodeList(TRADE_DSD, listName="CL_UNIT")
