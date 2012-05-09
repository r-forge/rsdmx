## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Created on 05/05/2011
## Last Update:09/05/2012
## Type: Function
## Name: getCodeListNames.R
## Description: A R function to retrieve the codelist names (dimension, attribute or all) from a DSD
##			  (SDMX Data Structure Definition). When retrieving all codelists, these can be retrieved
##			  in a list distinguishing both "dimension" vs. "attribute" codelists
## =================================================================================================

getCodeListNames<-function(sdmx, type=c('all','dimension','attribute'), split = FALSE){
	
	# type argument check
	type<-match.arg(type)

	# get Dimension codelists
	listDimensionsXML<-getNodeSet(sdmx,'//structure:Dimension')
	listDimensions<-sapply(listDimensionsXML,function(x) xmlGetAttr(x,'codelist'))

	# get Attribute codelists
	listAttrsXML<-getNodeSet(sdmx,'//structure:Attribute')
	listAttrs<-sapply(listAttrsXML,function(x) xmlGetAttr(x,'codelist'))

	# output management
	if(missing(type) || type=='all'){
		if(split==FALSE){
			listNames<-append(listDimensions, listAttrs)
		}else{
			listNames<-list(dimensions = listDimensions, attributes = listAttrs)
		}
	}else if(type=='dimension'){
		listNames<-listDimensions
	}else if(type=='attribute'){
		listNames<-listAttrs
	}
	
	return(listNames)
}