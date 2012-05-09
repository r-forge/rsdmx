
### UNSD-MDG (http://www.devinfo.info/Registry/UNSDMDG/Default.aspx)
library(RSDMX)

indicators<-read.sdmx(url="http://www.devinfo.info/Registry/UNSDMDG/download.aspx?isurl=True&fileId=Global/indicators.xml")
indicators.cd <- getCodeList(indicators)

units<-read.sdmx(url="http://www.devinfo.info/Registry/UNSDMDG/download.aspx?isurl=True&fileId=Global/units.xml")
units.cd <- getCodeList(units)
head(units.cd)
getCodeListNames(units)
units.list<-getCodeList(units, listName="CL_UNIT")
units.list

location<-read.sdmx(url="http://www.devinfo.info/Registry/UNSDMDG/download.aspx?isurl=True&fileId=Global/location.xml")
location.cd <- getCodeList(location)
head(location.cd)
location.list <- getCodeList(location, listName="CL_LOCATION")
location.list 

# Role<-read.sdmx(url="http://www.devinfo.info/Registry/UNSDMDG/download.aspx?isurl=True&fileId=Global/Role.xml")
# Role.cd <- getCodeList(Role)
# Role.cd
# 
# Institution<-read.sdmx(url="http://www.devinfo.info/Registry/UNSDMDG/download.aspx?isurl=True&fileId=Global/Institution.xml")
# Institution.cd <- getCodeList(Institution)
# Institution.cd

Domains<-read.sdmx(url="http://www.devinfo.info/Registry/UNSDMDG/download.aspx?isurl=True&fileId=Global/Domains.xml")
Domains.cd <- getCodeList(Domains)
Domains.cd

a<-getCodeList(Domains, lang="ES")
Domains.cd.es <- getCodeList(Domains, lang="ES")
Domains.cd.es


#Last Test for getCodeList
aqua<-read.sdmx(url="http://www.fao.org/figis/sdmx/registry/datastructure/FAO/AQUACULTURE_DATASTRUCTURE/0.1")
getCodeListNames(aqua)
getCodeList(aqua,c("CL_FAO_MAJOR_AREA","CL_UNIT","CL_ENVIRONMENT"))
getCodeList(aqua,c("CL_FAO_MAJOR_AREA","CL_UNIT","CL_ENVIRONMENT","CL_SDMX"))
getCodeList(aqua,c("CL_FAO_MAJOR_AREA","CL_UNIT","CL_ENVIRONMENT","CL_SDMX","CL_NAME"),"fr")
getCodeList(aqua,c("CL_FAO_MAJOR_AREA","CL_UNIT","CL_ENVIRONMENT"))$CL_ENVIRONMENT
getCodeList(aqua,"CL_ENVIRONMENT")
getCodeList(aqua,"CL_UNIT_MULTIPLIER") # issue of duplicated structure:Code values (to see with OpenSDMX developers)
