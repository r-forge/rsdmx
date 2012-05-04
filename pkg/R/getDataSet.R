## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Created on 23/04/2011
## Last Update: 04/05/2012
## Type: Function
## Name: getDataSet.R
## Description: A R function to retrieve a SDMX dataset in a R dataframe
## 			  This function replaces getSerie.R (deprecated)
## =====================================================================


getDataSet<-function(sdmx){
	
	#tag prefix management
	prefix1<-unlist(strsplit(xmlName(xmlRoot(sdmx)[[2]], full=T),':'))[1]
	prefix2<-unlist(strsplit(xmlName(xmlChildren(getNodeSet(sdmx,paste('//',prefix1,':DataSet', sep=''))[[1]])[[1]], full = T),':'))[1]
	
	#concepts (attributes)
	conceptsXML<-getNodeSet(sdmx, paste("//",prefix2,":SeriesKey/",prefix2,":Value", sep=""))
	concepts<-unique(sapply(conceptsXML, function(x) xmlGetAttr(x, "concept")))
	
	#series
	seriesXML<-getNodeSet(sdmx, paste('//',prefix2,':Series', sep=''))
	seriesNb<-length(seriesXML)
	

	#converting SDMX series to a DataFrame R object
	for(x in 1:seriesNb){
		
		# Single serie XMLInternalNode converted into a XMLInternalDocument
		serieXML<-xmlDoc(seriesXML[[x]])
		
		#obsTimes
		obsTimesXML<-getNodeSet(serieXML, paste("//",prefix2,":Series/",prefix2,":Obs/",prefix2,":Time", sep=""))
		obsTime<-sapply(obsTimesXML,function(x) {xmlValue(x)})
		L<-length(obsTime)

		
		#Concept values (Note: the SeriesKey (concept attributes/values) are duplicated according to the number of Time observations)
		conceptValues<-as.data.frame(sapply(concepts, function(x){
														conceptValuesXML<-getNodeSet(serieXML, sprintf(paste("//",prefix2,":SeriesKey/",prefix2,":Value[@concept='%s']",sep=""),x))
														conceptValues<-lapply(conceptValuesXML,function(i) {rep(xmlGetAttr(i,"value"),L)})
														}))

		
		#single Serie as DataFrame
		serieDF<-cbind(conceptValues, obsTime)
		
		#add single DataFrame to dataset 
		if(x==1){ dataset<-serieDF }else{ dataset<-rbind(dataset,serieDF)}
	}
	
	# adding obsValues
	obsValuesXML<-getNodeSet(sdmx,paste("//",prefix2,":ObsValue[@value]",sep=""))
	obsValue<-sapply(obsValuesXML, function(x) {xmlGetAttr(x,"value")})
	
	dataset<-cbind(dataset, obsValue)
	return(dataset)
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

