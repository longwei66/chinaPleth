## =============================================================================
##
##      Scrape Weather hourly data from weatherbase
##
##      For the cities : Shanghai, Beijing, Chengdu, Guangzhou, Shenyang, Paris
##
## =============================================================================

## Needed libraries and system configuration
library(XML)
library("rvest")
library("dplyr")
library("lubridate")
Sys.setlocale(category = "LC_ALL", locale = "C")

## =============================================================================
## Data Configuration
## =============================================================================

## City list and associated Url
uriCityWeather <-
        data.frame(
                city = c(
                        "Shanghai",
                        "Beijing",
                        "Chengdu",
                        "Guangzhou",
                        "Shenyang",
                        "Paris"
                ),
                url = c(
                        "https://www.wunderground.com/history/airport/ZSSS/",
                        "https://www.wunderground.com/history/airport/ZBAA/",
                        "https://www.wunderground.com/history/airport/ZUUU/",
                        "https://www.wunderground.com/history/airport/ZGGG/",
                        "https://www.wunderground.com/history/airport/ZYTX/",
                        "https://www.wunderground.com/history/airport/LFPO/"
                ))
## Plan number of days per year
nbDay <- 365

## Plan for data collection per year and city
listOfCityYear <- data.frame(
        year = c(
                "2015","2015","2015","2015","2015","2015",
                "2014","2014","2014","2014","2014","2014",
                "2013","2013","2013","2013","2013","2013"),
        city = c(
                "Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris",
                "Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris",
                "Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris"
                 )
)

## Prepar master data Frame with all data
masterWeather <- data.frame(matrix(nrow = 1, ncol = 16))
names(masterWeather) <- c(
                         "City", # City Name
                         "date.time",  # Date and time, local in PosixLt
                         "date_obs", # Date in chr
                         "Local.Time", # local time in AM/PM format
                         "Temperature", # Temperature in degree C
                         "Dewpoint", # Temperature in degree C
                         "Humidity", # Humidity in %
                         "Pressure", # Pressure in hPa
                         "Visibility", # in km
                         "Wind.Direction.code", # NSEW
                         "Wind.Direction.degree", # in degrees
                         "Wind.Speed", # in km/h
                         "Gust.Speed", # in km/h
                         "Precipitation", # ??? 0
                         "Events", # chr textual
                         "Conditions" #chr text
)
# Convert date in posixlt
masterWeather$date.time <- as.POSIXlt(masterWeather$date.time)


## =============================================================================
##      Scrap the data
## =============================================================================

## Loop per each year of the plan
for (j in 1:nrow(listOfCityYear)) {

        # Dataframe for storing city weather
        weather <- data.frame(matrix(nrow = 1, ncol = 16))
        names(weather) =  c(
                "City",
                "date_obs",
                "date.time",
                "Local.Time",
                "Temperature",
                "Dewpoint",
                "Humidity",
                "Pressure",
                "Visibility",
                "Wind.Direction.code",
                "Wind.Speed",
                "Gust.Speed",
                "Precipitation",
                "Events",
                "Conditions",
                "Wind.Direction.degree"
        )
        # Convert date in posixlt
        weather$date.time <- as.POSIXlt(weather$date.time)

        # Starting Date
        year <- listOfCityYear[j,]$year
        day <- paste(year,"-12-31", sep="")
        date <- strptime(day, "%Y-%m-%d")

        ## LOOP nbDay days
        for (i in 1:nbDay) {
                ## Create data for this loop
                day <- gsub(pattern = "-", replacement = "/", as.character(date))
                city <- listOfCityYear[j,]$city
                year <- listOfCityYear[j,]$year
                fileUrl <-paste(uriCityWeather[uriCityWeather$city == city,]$url,day,"/DailyHistory.html?format=1",sep = "")
                        
                # Open directly the csv file from the web site
                DayWeather <- read.csv(fileUrl, na.strings = c("-9999.0", "N/A"))

                ## Continue if table is not empty
                if (nrow(DayWeather) > 0) {
                        names(DayWeather) =  c(
                                "Local.Time",
                                "Temperature",
                                "Dewpoint",
                                "Humidity",
                                "Pressure",
                                "Visibility",
                                "Wind.Direction.code",
                                "Wind.Speed",
                                "Gust.Speed",
                                "Precipitation",
                                "Events",
                                "Conditions",
                                "Wind.Direction.degree",
                                "Date.UTC") 
                        DayWeather <- dplyr::select(DayWeather, -Date.UTC)
                                
                        ## fill data for date and city
                        DayWeather$date.time <- NA
                        DayWeather$City <- city
                        DayWeather$date_obs <- day
                                
                        DayWeather$date.time <-paste(DayWeather$date_obs,DayWeather$Local.Time)
                        DayWeather$date.time <-strptime(DayWeather$date.time,format = "%Y/%m/%d %I:%M %p")
                                
                                
                        ## Clean Data
                        #DayWeather$Temperature <- as.numeric(gsub(pattern = "(.*) \302\260C","\\1",DayWeather$Temperature))

                        # Dewpoint in degree C
                        #DayWeather$Dewpoint <-as.numeric(gsub(pattern = "(.*) \302\260C","\\1",DayWeather$Dewpoint))

                        # Humidity in %
                        #DayWeather$Humidity <- as.numeric(gsub(pattern = "(.*) %","\\1",DayWeather$Humidity))

                        # Pressure in hPa
                        #DayWeather$Barometer <- as.numeric(gsub(pattern = "(.*) hPa","\\1",DayWeather$Barometer))

                        # Visibility in km
                        #DayWeather$Visibility <-as.numeric(gsub(pattern = "(.*) km","\\1",DayWeather$Visibility))

                        # Wind direction
                        #DayWeather$Wind.Direction.degree <-as.numeric(gsub(pattern = ".* \\((.*)\302\260\\)","\\1",DayWeather$Wind.Direction.code))
                        #DayWeather$Wind.Direction.code <- gsub(pattern = "(.*) \\(.*\260\\)","\\1", DayWeather$Wind.Direction.code)
                                
                        # Wind Speed
                        #DayWeather$Wind.Speed <- as.numeric( gsub(pattern = "(.*) km/h","\\1",DayWeather$Wind.Speed))
                                
                        # Gust speed
                        #DayWeather$Gust.Speed <- as.numeric( gsub(pattern = "(.*) km/h","\\1",DayWeather$Gust.Speed))
                                
                        # Bind with previous data records
                        weather <- rbind(weather, DayWeather)
                                
                        # Success message
                        message(paste(city,"/",date,":",nrow(DayWeather),
                                                "records @", fileUrl))
                }
                else {
                        # Error message
                        message(paste(city,"/",date,"Failed ...    @",fileUrl))
                }
               
                # retract one day
                date <- date - 60 * 24 * 60
        }
        
        weather <- dplyr::select(weather,
                            City, # City Name
                            date.time,  # Date and time, local in PosixLt
                            date_obs, # Date in chr
                            Local.Time, # local time in AM/PM format
                            Temperature, # Temperature in degree C
                            Dewpoint, # Temperature in degree C
                            Humidity, # Humidity in %
                            Pressure, # Pressure in hPa
                            Visibility, # in km
                            Wind.Direction.code, # NSEW
                            Wind.Direction.degree, # in degrees
                            Wind.Speed, # in km/h
                            Gust.Speed, # in km/h
                            Precipitation, # ??? 0
                            Events, # chr textual
                            Conditions #chr text
        )

        ## Write temporary result
        write.csv(weather,
                file = paste(
                        '../data/chinaPlethData/local/weather/wunderground/',year, "-","weather-", city, '.csv',
                        sep = ""))
        ## Bind with master data
        masterWeather <- rbind(masterWeather, weather)
}

## =============================================================================
##      Save final Data
## =============================================================================
save(masterWeather, file = '../data/chinaPlethData/local/weather/wunderground/masterWeather.Rda')
write.csv(masterWeather, file = '../data/chinaPlethData/local/weather/wunderground/masterWeather.csv')

## =============================================================================
##      Data Cleaning
## =============================================================================
# reload data
load(file = '../data/chinaPlethData/local/weather/wunderground/masterWeather.Rda')
# City
masterWeather <- masterWeather[!is.na(masterWeather$City),]
# Temperature
masterWeather[masterWeather$Temperature == -9999,]$Temperature <- NA
# Temperature
masterWeather[masterWeather$Dewpoint == -9999 & !is.na(masterWeather$Dewpoint),]$Dewpoint <- NA
# Pressure
masterWeather[(masterWeather$Pressure == -9999 | masterWeather$Pressure == 101) & !is.na(masterWeather$Pressure),]$Pressure <- NA
# Wind Speed
masterWeather[masterWeather$Wind.Speed == "Calm" & !is.na(masterWeather$Wind.Speed),]$Wind.Speed <- "0"
masterWeather$Wind.Speed <- as.numeric(masterWeather$Wind.Speed)
masterWeather[(masterWeather$Gust.Speed == "-" | masterWeather$Gust.Speed == "-") & !is.na(masterWeather$Gust.Speed),]$Gust.Speed <- NA
masterWeather$Gust.Speed <- as.numeric(masterWeather$Gust.Speed)

masterWeather[masterWeather$Conditions == "",]$Conditions <- "Unknown"

## =============================================================================
##      Plot Humidity
## =============================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_line(aes(x = date.time, y = Humidity, col = Humidity))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Humidity in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(date.time = min(masterWeather$date.time), Humidity = 50,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = date.time, y = Humidity), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g


## =============================================================================
##      Plot Temperature
## =============================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_line(aes(x = date.time, y = Temperature, col = Humidity))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Temperature in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(date.time = min(masterWeather$date.time), Temperature = -22,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = date.time, y = Temperature), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g

## =============================================================================
##      Plot Pressure
## ===========================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_line(aes(x = date.time, y = Pressure, col = Humidity))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Atmospheric pressure in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(date.time = min(masterWeather$date.time), Temperature = 1024,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = date.time, y = Temperature), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g

## =============================================================================
##      Plot Temperature vs. Pressure
## ===========================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = Temperature, y = Pressure, col = Humidity))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Atmospheric pressure vs. Temperature in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(Temperature = -20, Pressure = 980,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = Temperature, y = Pressure), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g


## =============================================================================
##      Plot Dewpoint vs. Pressure
## ===========================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = Dewpoint, y = Pressure, col = Humidity))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Atmospheric pressure vs. Dewpoint in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(Dewpoint = -20, Pressure = 980,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = Dewpoint, y = Pressure), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g

## =============================================================================
##      Plot Dewpoint vs. Temperature
## ===========================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = Dewpoint, y = Temperature, col = Humidity))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Temperature vs. Dewpoint in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(Dewpoint = -20, Temperature = -20,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = Dewpoint, y = Temperature), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g

## =============================================================================
##      Plot Humidity vs. Temperature
## ===========================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = Humidity, y = Temperature, col = Pressure))
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=1024, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Temperature vs. Dewpoint in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(Humidity = 50, Temperature = -20,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = Humidity, y = Temperature), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g

## =============================================================================
##      Plot Visibility
## =============================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = date.time, y = Visibility, col = Humidity), alpha = 0.7)
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Visibility in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(date.time = min(masterWeather$date.time), Visibility = 5,lab = "Text",
                       City = factor("Beijing",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = date.time, y = Visibility), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g


## =============================================================================
##      Plot Gust.Speed
## =============================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = date.time, y = Gust.Speed, col = Humidity), alpha = 0.7)
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Gust.Speed in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(date.time = min(masterWeather$date.time), Gust.Speed = 60,lab = "Text",
                       City = factor("Shenyang",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = date.time, y = Gust.Speed), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g


## =============================================================================
##      Plot Wind.Direction.degree vs. Temperature
## =============================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_point(aes(x = Temperature, y = Wind.Direction.degree, col = Humidity), alpha = 0.7)
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Temperature vs. Wind direction in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ann_text <- data.frame(Temperature = min(masterWeather$Temperature), Wind.Direction.degree = 60,lab = "Text",
                       City = factor("Shenyang",levels = unique(weather$City)))
g <- g + geom_text(data = ann_text, aes(x = Temperature, y = Wind.Direction.degree), 
                   label = "(c) chinaPleth.io
Data from
wunderground.comm", col = "black", size = 3, hjust = 0)
g


## =============================================================================
##      Plot Wind.Direction.degree vs. Temperature
## =============================================================================
g <- ggplot(data = masterWeather)
g <- g + geom_bar(aes(x = Wind.Direction.code, fill = Wind.Direction.code), stat = "count")
g <- g + facet_grid(facets = . ~ City)
g <- g + scale_color_gradient2(midpoint=50, low="blue", mid="green",high="red", space ="Lab" )
g <- g + ggtitle("Temperature vs. Wind direction in few cities during the last 3 year")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g



## =============================================================================
##      Conditions
## =============================================================================
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)

makeCloudConditions <- function(dF,city) {
        writeLines(text = dF[dF$City == city,]$Conditions, con = 'temp/test.txt')
        path <- system.file("temp/", package = "RXKCD")
        datafiles <- list.files(path)
        xkcd.df <- read.csv(file = 'temp/test.txt')
        xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df[, 1])))
        xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
        xkcd.corpus <- tm_map(xkcd.corpus, content_transformer(tolower))
        xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
        tdm <- TermDocumentMatrix(xkcd.corpus)
        m <- as.matrix(tdm)
        v <- sort(rowSums(m),decreasing=TRUE)
        d <- data.frame(word = names(v),freq=v)
        pal <- brewer.pal(9, "BuGn")
        pal <- pal[-(1:2)]
        png(paste("wordcloud1-",city,".png",sep=""), width=1280,height=800)
        wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
        dev.off()
        
        # 2nd Type
        d <- dF[dF$City == city,]
        d$Conditions <- as.factor(d$Conditions)
        d$date.time <- as.character(d$date.time)
        d <- d %>% group_by(Conditions)
        d <- d %>% summarise_each(funs(sum(!is.na(.))))
        
        png(paste("wordcloud2-",city,".png",sep=""), width=800,height=800)
        wordcloud(d$Conditions,d$City, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
        dev.off()
        
        # 3rd Type
        pal2 <- brewer.pal(8,"Dark2")
        png(paste("wordcloud3-",city,".png",sep=""), width=800,height=800)
        wordcloud(d$Conditions,d$City, scale=c(8,.7),min.freq=3,
                  max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
        dev.off()
        
        # 4rd Type
        pal2 <- brewer.pal(8,"Dark2")
        png(paste("wordcloud4-",city,".png",sep=""), width=800,height=800)
        wordcloud(d$Conditions,d$City,   scale=c(8,0.7),min.freq=2,
                  max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
        dev.off()
        
        # 5th Type
        pal2 <- brewer.pal(8,"Dark2")
        png(paste("wordcloud4-",city,".png",sep=""), width=800,height=800)
        wordcloud(d$Conditions,d$City,  scale=c(8,0.7),min.freq=2,
                  max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
        dev.off()
        
        unlink('temp/test.txt')
}

for (i in unique(masterWeather$City)){
        makeCloudConditions(masterWeather,i)
}


### 
## http://api.openweathermap.org/data/2.5/forecast/city?id=1796236&APPID=542ed4ef6bcade8ef6a87ad6e4eee335
##
library(jsonlite)
# function which return weather forecast for Shanghai
# use google maps elevation API
getWeatherForecast <- function() {
        
        # Openweathermap forecast API
        Url <- "http://api.openweathermap.org/data/2.5/forecast/city?id=1796236&APPID=542ed4ef6bcade8ef6a87ad6e4eee335"
        jsonData <- fromJSON(Url)
        forecast <- as.data.frame(jsonData)
}
## get elevation for all aiports
