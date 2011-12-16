



getKeyFamily.CodeList <-function(x) {
#   indKeyfa <-which(xmlSApply(x, xmlName)=="KeyFamilies")
#   return(xmlSApply(x, xmlValue)[indKeyfa])
##no xpath
#   xmlSApply(keyfa[["KeyFamilies"]][["KeyFamily"]], xmlValue)["Name"]

##no xpath
  strucName <- getNodeSet(x, "//structure:KeyFamily/structure:Name")
  ret<-sapply(strucName, xmlValue)
  if(length(ret)>1) warning("DSD contains more than 1 keyfamily, not supported yet\n")

  return(ret)

}


getKeyFamily.DataSet <-function(x) {
  strucName <- getNodeSet(x, "//generic:KeyFamilyRef")
  ret<-sapply(strucName, xmlValue)
  if(length(ret)>1) warning("DSD contains more than 1 keyfamily, not supported yet\n")

  return(ret)

}

setGeneric(name = "getKeyFamily", def = function (x){ standardGeneric ( "getKeyFamily" )})

setMethod( f = "getKeyFamily" , signature = "RSDMXCodeLists",
  def = function(x) getKeyFamily.CodeList(x)
)

setMethod( f = "getKeyFamily" , signature = "RSDMXKeyFamilies",
  def = function(x) getKeyFamily.CodeList(x)
)

setMethod( f = "getKeyFamily" , signature = "RSDMXDataSet",
  def = function(x) getKeyFamily.DataSet(x)
)