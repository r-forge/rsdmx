## SERIE example1
library(RSDMX)
serie<-system.file("exampleData", "capture.xml", package= "RSDMX")
serieTP<-xmlTreeParse(serie, useInternalNodes = TRUE)
df<-getSerie(serieTP)
df[1:20,]




## using reshape package to rearrange data:
library(reshape2)
df1<-df[1:1000,]
head(df1)

## data is already 'molten'
co<-colnames(df1)
df_melt<-melt(df1, id = co[1:3], measured = co[4:5])
head(df_melt)


m1<-melt(df1)
head(m1)

dim(df_melt)

##
df1<-rename(df1, c(obsValue="value"))

colnames(df1)<-gsub("_",".",co)
colnames(df1)

head(df1)
sapply(df1,class)
levels(df1[,"obsTime"])
unique(df1[,"value"])

df1[,"value"] <-as.numeric(as.character(df1[,"value"]))

df2<-dcast(df1, ...~ obsTime)
head(df2)
dim(df2)


