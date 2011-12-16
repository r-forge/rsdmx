getDimOld<-function(x, xPath=TRUE) {
  if(xPath) {
    cRef<-getNodeSet(x, "//structure:Dimension[@conceptRef]")
    concRefs <- sapply(cRef, function(x) xmlGetAttr(x, "conceptRef"))
    names(concRefs) <- paste("Dim", 1:length(concRefs), sep="_")
    return(concRefs)
  } else {
    indexDim<- which(xmlSApply(x[["KeyFamilies"]][["KeyFamily"]][["Components"]], xmlName)=="Dimension")
    dimList <-xmlSApply(x[["KeyFamilies"]][["KeyFamily"]][["Components"]], xmlAttrs)[indexDim]
    concRefs<-sapply(dimList, function(x) x["conceptRef"])
    names(concRefs) <- paste("Dim", 1:length(indexDim), sep="_")
    return(concRefs)
  }
}


hasKeyFamily<-function(x) {
  keyf<-getNodeSet(x, "//message:KeyFamilies")
  ret <- if(length(keyf)==0) FALSE else TRUE
  return(ret)
}

getDim.RSDMXS4<-function(x) {
    if(hasKeyFamily(x)){
      cRef<-getNodeSet(x, "//structure:Dimension[@conceptRef]")
      concRefs <- sapply(cRef, function(x) xmlGetAttr(x, "conceptRef"))
      names(concRefs) <- paste("Dim", 1:length(concRefs), sep="_")
    } else {
      concRefs<- NULL
      warning("Object has not keyfamily info\n")
    }
    return(concRefs)
}

setGeneric(name = "getDim", def = function (x){ standardGeneric ( "getDim" )})

setMethod( f = "getDim" , signature = "RSDMXCodeLists",
  def = function(x) getDim.RSDMXS4(x)
)

setMethod( f = "getDim" , signature = "RSDMXKeyFamilies",
  def = function(x) getDim.RSDMXS4(x)
)

setMethod( f = "getDim" , signature = "RSDMXDataSet",
  def = function(x) cat("getDim not available for dataSet\n")
)

setMethod( f = "getDim" , signature = "RSDMX",
  def = function(x) cat("getDim not available for dataSet\n")
)
