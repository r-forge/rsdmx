getTimeDim <-function(x) {
  cRef<-getNodeSet(x, "//structure:TimeDimension[@conceptRef]")
  concRefs <- sapply(cRef, function(x) xmlGetAttr(x, "conceptRef"))
  if(length(concRefs )==0) concRefs  <- NULL
  return(concRefs)
}
