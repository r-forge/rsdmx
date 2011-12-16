## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Creation: 05/05/2011
## Last update: -

######### getCodeListNames(sdmx) ###################

getCodeListNames<-function(sdmx)
{
listNamesTP<-getNodeSet(sdmx,"//structure:CodeList")
listNames<-sapply(listNamesTP,function(x) xmlGetAttr(x,"id"))
return(listNames)
}