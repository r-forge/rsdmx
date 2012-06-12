## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Last update: 12/06/2012
##
######### getCodeList(file,listName,lang) ###################

getCodeList <- function(sdmx, listName, lang){
	
	# all codelist names (apply to complete DSDs potentially containing multiple codelists)
	cln <- getCodeListNames(sdmx) 
	
	# prepare listNames & wrongListNames objects
	listNames <- NULL
	wrongListNames <- NULL
	
	if (missing(listName) || listName=="all") { 
		listNames <- cln
		
	} else {
		exist <- listName %in% cln
		df <- data.frame(listName, exist)
		listNames <- as.vector(df$listName[df$exist==TRUE])
		wrongListNames <- as.vector(df$listName[df$exist==FALSE])
	}

	# Parse SDMX codelists
	#---------------------
	if (length(listNames)>=1){
		
		result <- NULL
		for (listName in listNames) {
  
			# initialize single listName result
			singleResult <- NULL
			
			# List of values
			cList<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code",listName))
			value<-sapply(cList, function(x) xmlGetAttr(x, "value"))
  
			# List of Descriptions (languages)
			cDes<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value]/structure:Description[@xml:lang]",listName))
			desList<-unique(sapply(cDes, function(x) xmlGetAttr(x,"xml:lang")))
			
			# 
			
			# descriptions object
			if(missing(lang) || lang=="all"){ descriptions <- desList} else{ descriptions <- lang}
		
			# dataframe of descriptions (structure:Description)
			if(is.null(unlist(desList))){
				singleResult <- value
			
			}else{
				for (description in descriptions) {
				
					parsedResult <- NULL
					if(description %in% desList){
						# parse the result from XML
						parsedResult <- sapply(value, function(value) {
														des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']", listName, value, description))
														desResult<-sapply(des,function(x) { val<- xmlValue(x,encoding="UTF-8") })
													})
					}
				
					# look if the description is available for the codelist
					if(is.null(parsedResult)){
						if(length(listNames)==1){
							stop("The codelist ", listName, " does not have description named ", description)
						}else{
							warning("The codelist ", listName, " does not have description named ", description)
						}
					
					
					}else{
						# processing the single codelist
						codeList<-as.data.frame(as.matrix(parsedResult))
						colnames(codeList)<-description
						
						# merge different descriptions
						if (length(singleResult) == 0) {
							singleResult <- codeList
						} else {
							singleResult <- cbind(singleResult, codeList)
						}
					}
				}
			}
			
			
			singleResult<-list(singleResult)
			names(singleResult)<-listName
			
			# append to final result
			if (length(result) == 0) {
				result <- singleResult
			} else {
				result <- append(result, singleResult)
			}

		}
  
		# Single codelist (to avoid returing a list and return a dataframe)
		if(length(listNames)==1){ result <- result[[1]] }
  
		# Result (or Result + warning dataframe)   
		if (length(wrongListNames) >= 1){  

			for (wrongListName in wrongListNames){
				warning("No codelist named ", wrongListName)
			}
			return(result)

		}else{
			return(result)
		}	
	
	}else{
		for (wrongListName in wrongListNames){
			stop("No codelist named ", wrongListName)
		}
	}
	
}#end function