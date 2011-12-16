## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Created on 23/04/2011
## Last Update: 26/04/2011
## Description: An example function to eetrieve a SDMX dataset in a R dataframe
##
## TO BE INTEGRATED:
## - different ways of returning the data in the DF (one column for obsTime + 1 for the value, or: various column for the value
## corresponding to each obsTime (colname) ) 
## - to investigate processing time (especially a better use of xPath,etc)
## ...

getSerie<-function(sdmx)
  {
  conceptListTP<-getNodeSet(sdmx,"//generic:SeriesKey/generic:Value")
  conceptList<-unique(sapply(conceptListTP, function(x) xmlGetAttr(x, "concept")))

  # quelles années sont intégrées dans le dataset??
#   cObsTime<-getNodeSet(sdmx, "//message:DataSet/generic:Series/generic:Obs/generic:Time")
  cObsTime<-getNodeSet(sdmx, "//generic:Series/generic:Obs/generic:Time") # mat says: I changed line since message:DataSet did not seem to be in FAOSTAT
  obsTime<-unique(sapply(cObsTime,function(x) {xmlValue(x)}))
  L<-length(obsTime)
  
  #conceptValues (the dataframe is replicated according to the number of years)
  conceptValues<-as.data.frame(sapply(conceptList, function(x)
    {
     cConceptValue<-getNodeSet(sdmx, sprintf("//generic:SeriesKey/generic:Value[@concept='%s']",x))
     conceptValue<-sapply(cConceptValue,function(x) {rep(xmlGetAttr(x,"value"),L)})
    }))

  #Valeurs de capture
  cObsValue<-getNodeSet(sdmx,"//generic:ObsValue[@value]")
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

