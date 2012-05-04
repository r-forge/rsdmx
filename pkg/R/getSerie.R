## !!! DEPRECATED !!!###
## REPLACED BY getDataSet.R

## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Created on 23/04/2011
## Last Update: 20/12/2011
## Description: An example function to eetrieve a SDMX dataset in a R dataframe
##
## - Manage now the tag prefix (if different than "generic")

## getSerie(sdmx) ##

getSerie<-function(sdmx){
	
	#tag prefix management
	prefix1<-unlist(strsplit(xmlName(xmlRoot(sdmx)[[2]], full=T),":"))[1]
	prefix2<-unlist(strsplit(xmlName(xmlChildren(getNodeSet(sdmx,paste("//",prefix1,":DataSet", sep=""))[[1]])[[1]], full = T),":"))[1]
	
	#concepts
	conceptListTP<-getNodeSet(sdmx, paste("//",prefix2,":SeriesKey/",prefix2,":Value", sep=""))
  	conceptList<-unique(sapply(conceptListTP, function(x) xmlGetAttr(x, "concept")))

	#obsTimes
  	cObsTime<-getNodeSet(sdmx, paste("//",prefix2,":Series/",prefix2,":Obs/",prefix2,":Time", sep=""))
  	obsTime<-unique(sapply(cObsTime,function(x) {xmlValue(x)}))
  	L<-length(obsTime)
  
  	#conceptValues (the dataframe is replicated according to the number of years)
  	conceptValues<-as.data.frame(sapply(conceptList, function(x){
    	cConceptValue<-getNodeSet(sdmx, sprintf(paste("//",prefix2,":SeriesKey/",prefix2,":Value[@concept='%s']",sep=""),x))
     	conceptValue<-sapply(cConceptValue,function(x) {rep(xmlGetAttr(x,"value"),L)})
    }))

  	#obsValues
  	cObsValue<-getNodeSet(sdmx,paste("//",prefix2,":ObsValue[@value]",sep=""))
  	obsValue<-sapply(cObsValue,function(x) {xmlGetAttr(x,"value")})
  	timeSerie<-cbind(conceptValues,obsTime,obsValue)

  	#check classes (HORRIBLE WORKAROUND)
  	modes<-sapply(timeSerie[1,], checkMode)
  	for(i in 1:ncol(timeSerie)) timeSerie[,i]<-if(modes[i]=="numeric") as.numeric(as.character(timeSerie[,i])) else timeSerie[,i]

  	return(timeSerie)
  }


#### Methods
setAs("RSDMXCodeLists", "data.frame", function(from) stop("Only datasets objects can be converted into data.frame\n"))
setAs("RSDMXDataSet", "data.frame", function(from) getSerie(from))

as.data.frame.RSDMXCodeLists <-function(x,..){
  stop("Only datasets objects can be converted into data.frame\n")
}

as.data.frame.RSDMXDataSet<-function(x,..){
  getSerie(x)
}


#### Workaround for converter
checkMode<-function(x){
  options(warn=-1)
  check<-as.numeric(as.character(x))
  options(warn=0)
  if(is.na(check)) {
    return("factor")
  } else {
    return("numeric")
  }
}

