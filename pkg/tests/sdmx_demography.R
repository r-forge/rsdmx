library(RSDMX)
demo.dsd.path <- system.file("exampleData", "demography","demography.xml", package = "RSDMX")
demogr <- read.sdmx(demo.dsd.path)
summary(demogr)


cRef<-getNodeSet(demogr, "//structure:Dimension[@conceptRef]")
if(length(cRef)==0)

demo.data.path <- system.file("exampleData", "demography","demography_xs.xml", package = "RSDMX")


demogr_dat <- read.sdmx(demo.data.path)
summary(demogr_dat)
