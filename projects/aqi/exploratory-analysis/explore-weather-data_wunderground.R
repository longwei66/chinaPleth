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
                   wunderground.com", col = "black", size = 3, hjust = 0)
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
