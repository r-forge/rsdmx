



as.sp <-function(x){

  ## x should be of class dataset
#   if(getSdmxType(x)!="euhemmanuelcheck... Dataset") stop("arg x should be ..\n")
  ## y should be of class codelist
  # check if y has codelist "UN_COUNTRY"
  #if("UN_COUNTRY"%in%getCodeListNames(y)) ## enelveé si pas de y
  # extract codelist with "UN_COUNTRY"

  # if(getSdmxType(y)!="euhemmanuelcheck... Dataset") stop("arg x should be ..\n")



  # convert to data frame: x to X
  X <- as.data.frame(x)
  if(!"UN_COUNTRY"%in%colnames(X)) stop("object x does not have UN_COUNTRY information\n")

  
  #check if X unique values associated to UN_COUNTRY
  if(any(duplicated(X[, "UN_COUNTRY"]))){ 
  # if duplicated:
  # a) user has specified conditions!!!
  # b) we average!!
print("aggregatoing\n")
  X <- aggregate(X[, "obsValue"], by=list(X[, "UN_COUNTRY"]), sum)
  colnames(X)<-c("UN_COUNTRY","obsValue")
  row.names(X)<-X$UN_COUNTRY

}


## spatial object
  require(maptools)
  require(sp)
  data(wrld_simpl)
  wo <- wrld_simpl

## map UN country to the shapefile...
  wrld_un<-spChFIDs(wo,as.character(wo$UN))
  id<-match(row.names(X),sapply(slot(wrld_un,"polygons"),function(x) slot(x,"ID")))
  id<-id[!is.na(id)]
  spdf<-SpatialPolygonsDataFrame(wrld_un[id,],X)

return(spdf)

}


### emannuel: quand finit donne exemple ici:

if(FALSE){
library(RSDMX)
url3<-"http://www.fao.org/figis/sdmx/repository/Data/FAO_PUBLISHMENT/CAPTURE/FAO/?startPeriod=2005&endPeriod=2006"
a<-read.sdmx(url=url3)
serie<-getSerie(a) 
spdf<-as.sp(a)

centroids<-SpatialPoints(t(sapply(slot(catchSPDF,"polygons"),function(x) {slot(x,"labpt")})))

}