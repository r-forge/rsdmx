## Author: Emmanuel Blondel
## Contact: emmanuel.blondel1 at gmail.com
## Last update: 09/05/2011
## Updates:
## - Encoding set to "UTF-8" by default (getEncoding makes R crashing when there is no Encoding XML attribute)
## - if loop set to distinguish codelist values with 1 description (1 language) from those with more than 1 description
## - if listName is missing --> list of all codelist (list of dataframes)
## - if the user selects a list of codeListNames, the function returns a list of dataframes in which each dataframe is the codeList content
##   A warning message is given if one of more listName is not correct. An error is given if all listNames are invalid
## - Else for codeLists without Description (e.g. CL_UN_COUNTRY) given as vector (to give as dataframe?)


######### getCodeList(file,listName,lang) ###################

getCodeList<-function(sdmx,listName,lang)
{
listNames<-getCodeListNames(sdmx)
if (length(listNames)==1) {listName<-listNames} # to avoid a single codeList to be returned in a list

if ((missing(listName) || listName=="all") && length(listNames)>1)
  {
  allCodeList=NULL
  
  for (i in 1:length(listNames)) {
    listName<-listNames[i]
    cList<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code",listName))
    value<-sapply(cList, function(x) xmlGetAttr(x, "value"))
  
    # List of Descriptions (languages)
    cDes<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value]/structure:Description[@xml:lang]",listName))
    desList<-unique(sapply(cDes, function(x) xmlGetAttr(x,"xml:lang")))
    
    if (missing(lang) || lang=="all")
      {
      if (length(desList)>1)
        {
        # dataframe of descriptions (structure:Description)
        codeList<-as.data.frame(t(sapply(value,function(value) {sapply(desList,function(x)
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,x))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })
          })))
        codeList<-list(codeList)
        names(codeList)<-listName
        }
      else if (length(desList)==1)
        {
        codeList<-as.data.frame(as.matrix(sapply(value,function(value) 
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,desList))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })))
        colnames(codeList)<-desList
        codeList<-list(codeList)
        names(codeList)<-listName
        }
      else
        {
        codeList<-list(as.data.frame(value))
        names(codeList)<-listName
        }  
      }
    else
      {
      if (lang %in% desList)
        {
        # dataframe of descriptions (structure:Description)
        codeList<-as.data.frame(as.matrix(sapply(value,function(value) 
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,lang))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })))
        colnames(codeList)<-lang
        codeList<-list(codeList)
        names(codeList)<-listName
        }
      else
        {
        if (length(desList)>=1){
          cat("Error: no description attribute with the value", lang, "for the codelist", listName,"\n")
          cat("The available codelist descriptions values (languages) are: \n")
          list<-as.data.frame(desList)
          colnames(list)<-"Description"
          return(list)
        } else {
          cat("Error: no description attribute with the value", lang, "for the codelist", listName,"\n")
          cat("The available codelist does not include descriptions. \n")
          }
        }
      }
    if (length(allCodeList)==0) {
      allCodeList<-codeList
      } 
    else {
      allCodeList<-append(allCodeList,codeList)
      }
    }
  return(allCodeList)
   
  }
else if (any(listName %in% listNames) && length(listName)>1)
  {
  #listName control
  exist<-listName %in% listNames
  df<-data.frame(listName,exist)
  selectListNames<-as.vector(df$listName[df$exist==TRUE])
  wrongListNames<-as.vector(df$listName[df$exist==FALSE])
  selectCodeList<-NULL
  
  for (i in 1:length(selectListNames)) {
    listName<-selectListNames[i]
  
    cList<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code",listName))
    value<-sapply(cList, function(x) xmlGetAttr(x, "value"))
  
    # List of Descriptions (languages)
    cDes<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value]/structure:Description[@xml:lang]",listName))
    desList<-unique(sapply(cDes, function(x) xmlGetAttr(x,"xml:lang")))
    
    if (missing(lang) || lang=="all")
      {
      if (length(desList)>1)
        {
        # dataframe of descriptions (structure:Description)
        codeList<-as.data.frame(t(sapply(value,function(value) {sapply(desList,function(x)
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,x))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })
          })))
        codeList<-list(codeList)
        names(codeList)<-listName  
        }
      else if (length(desList)==1)
        {
        codeList<-as.data.frame(as.matrix(sapply(value,function(value) 
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,desList))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })))
        colnames(codeList)<-desList
        codeList<-list(codeList)
        names(codeList)<-listName
        }
      else
        {
        codeList<-list(as.data.frame(value))
        names(codeList)<-listName
        }  
      }
    else
      {
      if (lang %in% desList)
        {
        # dataframe of descriptions (structure:Description)
        codeList<-as.data.frame(as.matrix(sapply(value,function(value) 
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,lang))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })))
        colnames(codeList)<-lang
        codeList<-list(codeList)
        names(codeList)<-listName
        }
      else
        {
        if (length(desList)>=1){
          cat("Error: no description attribute with the value", lang, "for the codelist", listName,"\n")
          cat("The available codelist descriptions values (languages) are: \n")
          list<-as.data.frame(desList)
          colnames(list)<-"Description"
          return(list)
        } else {
          cat("Error: no description attribute with the value", lang, "for the codelist", listName,"\n")
          cat("The available codelist does not include descriptions. \n")
          }
        }
      }
    if (length(selectCodeList)==0) {
      selectCodeList<-codeList
      } 
    else {
      selectCodeList<-append(selectCodeList,codeList)
      }
    }
  
  # Result (or Result + warning dataframe)   
  if (length(wrongListNames)>=1){  
    wrongListNames<-list(data.frame(wrongListNames))
    names(wrongListNames)<-"Warning"
    result<-append(selectCodeList,wrongListNames)
    return(result)
  }else{
    return(selectCodeList)
    }
  }
else if (length(listName)==1 && (listName %in% listNames))
  {                                                                                                  
  cList<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code",listName))
  value<-sapply(cList, function(x) xmlGetAttr(x, "value"))
  
  # List of Descriptions (languages)
  cDes<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value]/structure:Description[@xml:lang]",listName))
  desList<-unique(sapply(cDes, function(x) xmlGetAttr(x,"xml:lang")))
    
  if (missing(lang) || lang=="all")
      {
      if (length(desList)>1)
        {
        # dataframe of descriptions (structure:Description)
        codeList<-as.data.frame(t(sapply(value,function(value) {sapply(desList,function(x)
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,x))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })
          })))
        return(codeList)  
        }
      else if (length(desList)==1)
        {
        codeList<-as.data.frame(as.matrix(sapply(value,function(value) 
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,desList))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })))
        colnames(codeList)<-desList
        return(codeList)
        }
      else
        {
        codeList<-as.data.frame(value)
        return(codeList)
        }
      }
    else
      {
      if (lang %in% desList)
        {
        # dataframe of descriptions (structure:Description)
        codeList<-as.data.frame(as.matrix(sapply(value,function(value) 
          {
          des<-getNodeSet(sdmx, sprintf("//structure:CodeList[@id='%s']/structure:Code[@value='%s']/structure:Description[@xml:lang='%s']",listName,value,lang))
          sapply(des,function(x) {xmlValue(x,encoding="UTF-8")})
          })))
        colnames(codeList)<-lang
        return(codeList)
        }
      else
        {
        if (length(desList)>=1){
          cat("Error: no description attribute with the value", lang, "for the codelist", listName,"\n")
          cat("The available codelist descriptions values (languages) are: \n")
          list<-as.data.frame(desList)
          colnames(list)<-"Description"
          return(list)
        } else {
          cat("Error: no description attribute with the value", lang, "for the codelist", listName,"\n")
          cat("The available codelist does not include descriptions. \n")
          }
        }
      } 
  }   
else
  {
  exist<-listName %in% listNames
  df<-data.frame(listName,exist)
  wrongListName<-as.vector(df$listName[df$exist==FALSE])
  cat("Error: There is no codelist associated to the following names: \n")
  print(wrongListName)
  cat("Info: Use getCodeListNames(sdmx) to get the list of codelist names \n")
  }
}