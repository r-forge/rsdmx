#############################################################################
########### READ SDMX
#############################################################################



read.sdmx <-function(file, url, verbose=FALSE){

  if(!missing(file) & !missing(url))
    stop("Please provide either file or url, not both\n")

## local file
  if(!missing(file)){
    if(!file.exists(file)) stop("File ", file, "not found\n")
    ret<-xmlTreeParse(file, useInternalNodes = TRUE)
  } else if(!missing(url)) {
### url
    tmpf<- tempfile(pattern = "rsdmx")
    download.file(url, destfile=tmpf, quiet=!verbose)
    ret<-xmlTreeParse(tmpf, useInternalNodes = TRUE)
  }
### api: to be done

### manage result
  oldC<-class(ret)
  type<-getSdmxType(ret)
#    attr(ret, "type") <- type
  typeFull <- paste("RSDMX", type, sep="")
  new<-structure(ret, class=c(typeFull ,"RSDMX", oldC))
  return(new)
}





# setOldClass(c("RSDMX",  "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("RSDMXCodeLists","RSDMX",  "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("RSDMXDataSet","RSDMX",  "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("RSDMXKeyFamilies","RSDMX",  "XMLInternalDocument", "XMLAbstractDocument"))


setAs("RSDMX", "character", function(from) saveXML(from))



print.RSDMX =function(x, ...)
{
#   cat(as(x, "character"), "\n")
  cat("hello\n")
  NextMethod(XML:::print.XMLInternalDocument, x)
  
}

summary.RSDMX =function(object, ...) {
#   if(attr(object,"type")%in%c("CodeLists", "KeyFamilies")){
#     summary.RSDMX_codelist(object,...)
#   } else if(attr(object,"type")=="DataSet"){
#     summary.RSDMX_DataSet(object,...)
#   } else {
#     stop("type not recognised\n")
#   }
warning("Sorry, SDMX type not recorded\n")
}



summary.RSDMXCodeLists =function(object, ...)
{
  cat("###Object RSDMX###\n")
  cat("Key Family:\n\t-", getKeyFamily(object), "\n")
  cat("Type: \n\t-",getSdmxType(object),"\n")
  cat("Dimensions:\t \n")
  dims<-getDim(object)
  for(i in 1:length(dims))
    cat("\t-",dims[i]," \n")
  cat("Time dimensions:\t \n")
  Tdims<-getTimeDim(object)
  for(i in 1:length(Tdims))
    cat("\t-",Tdims[i]," \n")
  cat("Attributes:\t \n")
  attri<-getAttr(object)
  for(i in 1:length(attri))
    cat("\t-",attri[i]," \n")
  
}

summary.RSDMXKeyFamilies =function(object, ...) summary.RSDMXCodeLists(object,...)

summary.RSDMXDataSet =function(object, ...)
{
  cat("###Object RSDMX###\n")
  cat("Key Family:\n\t-", getKeyFamily(object), "\n")
  cat("Type: \n\t-",getSdmxType(object),"\n")
  cat("MORE INFOS to be shown??\n")
}



###### checks: ######
if(FALSE){
library(XML)
library(RSDMX)


a<-read.sdmx(url="http://www.fao.org/figis/sdmx/registry/KeyFamily/CAPTURE_KEYFAMILY/FAO/0.1")

class(a)
typeof(a)
mode(a)

print(a)
summary(a)

str(a)

## compare
tmpf<- tempfile(pattern = "fdoc")
download.file("http://www.fao.org/figis/sdmx/registry/KeyFamily/CAPTURE_KEYFAMILY/FAO/0.1", destfile=tmpf)
fdoc<-xmlTreeParse(tmpf, useInternalNodes = TRUE)

all.equal(summary(a), summary(fdoc))
all.equal(print(a), print(fdoc))
all.equal(xmlRoot(a), xmlRoot(fdoc))
all.equal(xmlNamespaceDefinitions(a), xmlNamespaceDefinitions(fdoc))
all.equal(xmlChildren(a), xmlChildren(fdoc))
all.equal(addChildren(a), addChildren(fdoc))


"http://www.fao.org/figis/sdmx/registry/DataStructure/CAPTURE_DSD/FAO/0.1"

restAPI(series="capture", type="KeyFamily",version="0.1", provider="FAO")




"http://www.fao.org/figis/sdmx/registry/DataStructure/CAPTURE_DSD/FAO/0.1"
"http://www.fao.org/figis/sdmx/registry/UtilitySchema/CAPTURE_UTILITY_SCHEMA/FAO/0.1"
}






#############################################################################
########### REST API
#############################################################################
restAPI <-function(series,type=c("DataStructure", "KeyFamily", "UtilitySchema"), version, provider,root="http://www.fao.org/figis/sdmx/registry/"){

## checks:
  type <- match.arg(type) # restricts type to belong only to pre-defined values
  series <-toupper(series) # Erik: is upper case standard in rest api?
  type2 <- switch(type, "DataStructure"="DSD", "KeyFamily"="KEYFAMILY", "UtilitySchema"="UTILITY_SCHEMA")

## creat:
  url <-paste(root, type, "/", series, "_", type2 , "/", provider, "/", version, sep="")
  return(url)
}


### checks:
if(FALSE){
restAPI(series="capture", version="0.1", provider="FAO")

restAPI(series="capture", type="KeyFamily",version="0.1", provider="FAO")

"http://www.fao.org/figis/sdmx/registry/KeyFamily/CAPTURE_KEYFAMILY/FAO/0.1"
"http://www.fao.org/figis/sdmx/registry/DataStructure/CAPTURE_DSD/FAO/0.1"
"http://www.fao.org/figis/sdmx/registry/UtilitySchema/CAPTURE_UTILITY_SCHEMA/FAO/0.1"
}