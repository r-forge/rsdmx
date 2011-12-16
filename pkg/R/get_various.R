getPriMes<-function(x) {
  cRef<-getNodeSet(x, "//structure:PrimaryMeasure[@conceptRef]")
  concRefs <- sapply(cRef, function(x) xmlGetAttr(x, "conceptRef"))
  if(length(concRefs )==0) concRefs  <- NULL
  return(concRefs)
}


getAttr<-function(x) {
#   indexAtt<- which(xmlSApply(x[["KeyFamilies"]][["KeyFamily"]][["Components"]], xmlName)=="Attribute")
#   attList <-xmlSApply(x[["KeyFamilies"]][["KeyFamily"]][["Components"]], xmlAttrs)[indexAtt]
#   att_concRefs<-sapply(attList, function(x) x["conceptRef"])
  cRef<-getNodeSet(x, "//structure:Attribute[@conceptRef]")
  att_concRefs <- sapply(cRef, function(x) xmlGetAttr(x, "conceptRef"))
  names(att_concRefs) <- paste("Att", 1:length(att_concRefs ), sep="_")
  return(att_concRefs)
}

getSdmxType <- function(x) {
  r<-xmlRoot(x)
  res <-xmlName(r[[2]])
  if(res=="Structures") res <-xmlName(r[[2]][[1]])
  recognised <- c("CodeLists", "DataSet", "KeyFamilies")
  look <- charmatch(tolower(res),tolower(recognised))
  if(is.na(look)) {
    warning("SDMX type: is ", res, " a recognised type?\n", sep="")
  } else {
    res <-recognised[look]
  }
  return(res)
}
