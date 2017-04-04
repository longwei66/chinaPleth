## -----------------------------------------------------------------------------
##                      LOAD REQUIRED PACKAGES
##
##      Nb : I had difficulties to have gpclib, sp and fortify working
##           I ended up upgrading all packages and install few linux deps.
## -----------------------------------------------------------------------------

library(maptools)
library(rgeos)
library(gpclib)
library(foreign)
library(sp)
library(ggplot2)
library(plyr)
library(dplyr)


## -----------------------------------------------------------------------------
##              CHINA MAP               Id in CHINESE
## -----------------------------------------------------------------------------
##      Generate the SpatialPolygonsDataFrame
##
##      input :
##              - a *.shp file of the region to map
##
##      sources :
##              - this can be downloaded from http://www.gadm.org/download
##              - eg. http://biogeo.ucdavis.edu/data/gadm2.7/shp/CHN_adm.zip
##              - once unzip the data to use depends on the level needed
##                      - *1.shp is state level
##                      - *2.shp is region level, etc..
##
##      output : for level 0 to 3
##              - ChinaLevel2Data : data associated to level subdivision
##              - ChinaLevel2dF : polygons shape as data frame
##              - Level2Centers : long / lat of centers of level divisions
##              - 
## -----------------------------------------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load GAA data : level 0
## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load SpatialPolygonsDataFrame
ChinaPolygonsLevel0 <- readShapeSpatial("../data/chinaPlethData/ext/GAA/CHN_adm0.shp")
## Clean the names
ChinaPolygonsLevel0@data$NAME_NONLA <- gsub("\\|.*$", "", ChinaPolygonsLevel0@data$NAME_NONLA)

# Convert the spacial polygon shapes to data frame
# Use level2 as index
ChinaLevel0Data <- ChinaPolygonsLevel0@data
ChinaLevel0Data$id <- ChinaLevel0Data$NAME_NONLA

# Fortify the data (polygon map as dataframe)
ChinaLevel0dF <- fortify(ChinaPolygonsLevel0, region = "NAME_NONLA")

## Getting the center of each subdivision (to plot names or others)
## ddply is from the plyr package
Level0Centers <-
        ddply(ChinaLevel0dF, .(id), summarize, clat = mean(lat), clong = mean(long))

## Merge results in one data frame
ChinaLevel0Data <- merge(ChinaLevel0Data, Level0Centers, all = TRUE)



## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load GAA data : level 1
## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load SpatialPolygonsDataFrame
ChinaPolygonsLevel1 <- readShapeSpatial("../data/chinaPlethData/ext/GAA/CHN_adm1.shp")
## Regex to remove the complex characters versions of city names
## This does not work so well as cmpl are time to time on the left side as well
ChinaPolygonsLevel1@data$NL_NAME_1 <- gsub("^.*\\|", "", ChinaPolygonsLevel1@data$NL_NAME_1)
ChinaPolygonsLevel1@data[grep("黑龍江省", ChinaPolygonsLevel1@data$NL_NAME_1),]$NL_NAME_1 <- "黑龙江"
ChinaPolygonsLevel1@data[grep("新疆维吾尔自治区", ChinaPolygonsLevel1@data$NL_NAME_1),]$NL_NAME_1 <- "新疆"
ChinaPolygonsLevel1@data[grep("内蒙古自治区", ChinaPolygonsLevel1@data$NL_NAME_1),]$NL_NAME_1 <- "内蒙古"
ChinaPolygonsLevel1@data[grep("宁夏回族自治区", ChinaPolygonsLevel1@data$NL_NAME_1),]$NL_NAME_1 <- "宁夏"
ChinaPolygonsLevel1@data[grep("广西壮族自治区", ChinaPolygonsLevel1@data$NL_NAME_1),]$NL_NAME_1 <- "广西"
ChinaPolygonsLevel1@data[grep("西藏自治区", ChinaPolygonsLevel1@data$NL_NAME_1),]$NL_NAME_1 <- "西藏"

# Convert the spacial polygon shapes to data frame
# Use level1 as index
ChinaLevel1Data <- ChinaPolygonsLevel1@data
ChinaLevel1Data$id <- ChinaLevel1Data$NL_NAME_1
 
# Fortify the data (polygon map as dataframe)
ChinaLevel1dF <- fortify(ChinaPolygonsLevel1, region = "NL_NAME_1")

## Getting the center of each subdivision (to plot names or others)
## ddply is from the plyr package
Level1Centers <-
        ddply(ChinaLevel1dF, .(id), summarize, clat = mean(lat), clong = mean(long))
## Merge results in one data frame
ChinaLevel1Data <- merge(ChinaLevel1Data, Level1Centers, all = TRUE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load GAA data : level 2
## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load SpatialPolygonsDataFrame
ChinaPolygonsLevel2 <- readShapeSpatial("../data/chinaPlethData/ext/GAA/CHN_adm2.shp")
## Regex to remove the complex characters versions of city names
## This does not work so well as cmpl are time to time on the left side as well
ChinaPolygonsLevel2@data$NL_NAME_2 <- gsub("\\|.*$", "", ChinaPolygonsLevel2@data$NL_NAME_2)

# Convert the spacial polygon shapes to data frame
# Use level2 as index
ChinaLevel2Data <- ChinaPolygonsLevel2@data
ChinaLevel2Data$id <- ChinaLevel2Data$NL_NAME_2

# Fortify the data (polygon map as dataframe)
ChinaLevel2dF <- fortify(ChinaPolygonsLevel2, region = "NL_NAME_2")

## Getting the center of each subdivision (to plot names or others)
## ddply is from the plyr package
Level2Centers <-
        ddply(ChinaLevel2dF, .(id), summarize, clat = mean(lat), clong = mean(long))
## Merge results in one data frame
ChinaLevel2Data <- merge(ChinaLevel2Data, Level2Centers, all = TRUE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Load GAA data : level 3
## ~~~~~~~~~~~~~~~~~~~~~~~~~ 
ChinaPolygonsLevel3 <- readShapeSpatial("../data/chinaPlethData/ext/GAA/CHN_adm3.shp")
## Regex to remove the complex characters versions of city names
## This does not work so well as cmpl are time to time on the left side as well
ChinaPolygonsLevel3@data$NL_NAME_3 <- gsub("\\|.*$", "", ChinaPolygonsLevel3@data$NL_NAME_3)

# Convert the spacial polygon shapes to data frame
# Use level2 as index
ChinaLevel3Data <- ChinaPolygonsLevel3@data
ChinaLevel3Data$id <- ChinaLevel3Data$NL_NAME_3

# Fortify the data (polygon map as dataframe)
ChinaLevel3dF <- fortify(ChinaPolygonsLevel3, region = "NL_NAME_3")

## Getting the center of each subdivision (to plot names or others)
## ddply is from the plyr package
Level3Centers <-
        ddply(ChinaLevel3dF, .(id), summarize, clat = mean(lat), clong = mean(long))

ChinaLevel3Data <- merge(ChinaLevel3Data, Level3Centers, all = TRUE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Check if everything 
## is fine by plotting it
## ~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfrow = c(2,2))
plot(ChinaPolygonsLevel0,  main = "China level 0")
plot(ChinaPolygonsLevel1,  main = "China level 1")
plot(ChinaPolygonsLevel2,  main = "China level 2")
plot(ChinaPolygonsLevel3,  main = "China level 3")



