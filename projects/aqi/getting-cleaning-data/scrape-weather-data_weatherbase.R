## =============================================================================
##      Scrape Weather hourly data from weatherbase
##      For the cities : Shanghai, Beijing, Chengdu, Guangzhou, Shenyang, Paris
## =============================================================================

## Needed libraries and system configuration
## -----------------------------------------
library(XML)
library(rvest)
library(dplyr)
library(lubridate)
library(data.table)
Sys.setlocale(category = "LC_ALL", locale = "C")

## Data Configuration
## -----------------------------------------

## Cities : city names and url to get weather
uriCityWeather <- read.csv(file = "./data/share/aqi/weather-cities-data-source-url.csv", 
                           sep = ";", 
                           header = TRUE)

## Configure scrapping plan, which cities, which year ?
listOfCityYear <- data.frame(
        year = c(
                "2016","2016","2016","2016","2016","2016" #,
                #"2015","2015","2015","2015","2015","2015" #,
                #"2014","2014","2014","2014","2014","2014" #,
                #"2013","2013","2013","2013","2013","2013"
                ),
        city = c(#
                "Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris" #,
                #"Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris" #,
                #"Shanghai","Beijing", "Chengdu","Guangzhou","Shenyang","Paris"#,
                #"Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris"
        )
)

# How many days should we scrape in each year
nbDay <- 365


## Loop per each year of the plant
for (j in 1:nrow(listOfCityYear)) {
        
        # Data.table for storing city weather
        weather <- as.data.frame(matrix(nrow = 1, ncol = 16))
        names(weather) <- c(
                "City",
                "date_obs",
                "date.time",
                "Local.Time",
                "Temperature",
                "Dewpoint",
                "Humidity",
                "Barometer",
                "Visibility",
                "Wind.Direction.code",
                "Wind.Speed",
                "Gust.Speed",
                "Precipitation",
                "Events",
                "Conditions",
                "Wind.Direction.degree")
        weather$date.time <- as.POSIXlt(weather$date.time)
        
        # Starting Date
        year <- listOfCityYear[j,]$year
        day <- paste(year,"-12-31", sep="")
        date <- strptime(day, "%Y-%m-%d")
        date_obs <- date
        
        ## LOOP 365 days
        for (i in 1:nbDay) {
                ## Create data frame with
                day <- as.character(date)
                city <- listOfCityYear[j,]$city
                year <- listOfCityYear[j,]$year
                fileUrl <-paste(uriCityWeather[as.character(uriCityWeather$city) == city,]$url,day,"&units=metric",sep = "")
                
                # Xpath to get the whole Table
                # //*[@id="left-weather-content"]/table[3]
                DayWeather <- fileUrl %>%
                        read_html() %>%
                        html_nodes(xpath = '//*[@id="left-weather-content"]/table[3]') %>%
                        html_table()
                
                ## Continue if table is not empty
                if (length(DayWeather) > 0) {
                        DayWeather <- as.data.frame(DayWeather)
                        names(DayWeather) =  c(
                                "Local.Time",
                                "Temperature",
                                "Dewpoint",
                                "Humidity",
                                "Barometer",
                                "Visibility",
                                "Wind.Direction.code",
                                "Wind.Speed",
                                "Gust.Speed",
                                "Precipitation",
                                "Events",
                                "Conditions"
                        )
                        
                        ## fill data for date and city
                        DayWeather$date.time <- NA
                        DayWeather$City <- city
                        DayWeather$date_obs <- day
                        
                        DayWeather$date.time <-paste(DayWeather$date_obs,DayWeather$Local.Time)
                        DayWeather$date.time <-strptime(DayWeather$date.time,format = "%Y-%m-%d %I:%M %p")
                        
                        
                        ## Clean Data
                        DayWeather$Temperature <- as.numeric(gsub(pattern = "(.*) \302\260C","\\1",DayWeather$Temperature))
                        
                        # Dewpoint in degree C
                        DayWeather$Dewpoint <-as.numeric(gsub(pattern = "(.*) \302\260C","\\1",DayWeather$Dewpoint))
                        
                        # Humidity in %
                        DayWeather$Humidity <- as.numeric(gsub(pattern = "(.*) %","\\1",DayWeather$Humidity))
                        
                        # Pressure in hPa
                        DayWeather$Barometer <- as.numeric(gsub(pattern = "(.*) hPa","\\1",DayWeather$Barometer))
                        
                        # Visibility in km
                        DayWeather$Visibility <-as.numeric(gsub(pattern = "(.*) km","\\1",DayWeather$Visibility))
                        
                        
                        # Wind direction
                        DayWeather$Wind.Direction.degree <-as.numeric(gsub(pattern = ".* \\((.*)\302\260\\)","\\1",DayWeather$Wind.Direction.code))
                        DayWeather$Wind.Direction.code <- gsub(pattern = "(.*) \\(.*\260\\)","\\1", DayWeather$Wind.Direction.code)
                        
                        # Wind Speed
                        DayWeather$Wind.Speed <- as.numeric( gsub(pattern = "(.*) km/h","\\1",DayWeather$Wind.Speed))
                        
                        # Gust speed
                        DayWeather$Gust.Speed <- as.numeric( gsub(pattern = "(.*) km/h","\\1",DayWeather$Gust.Speed))
                        
                        ## Bind with the past records
                        weather <- rbind(weather, DayWeather)
                        
                        # Success message
                        message(paste(city,"/",date,":",nrow(DayWeather),
                                      "records @", fileUrl))
                }
                else {
                        # Error message
                        message(paste(city,"/",date,"Failed ...    @",fileUrl))
                }
                # Bind with previous data records
                weather <- rbind(weather, DayWeather)
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
                                 Barometer, # Pressure in hPa
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
                          '../data/chinaPlethData/local/weather/weatherbase/',year, "-","weather-", city, '.csv',
                          sep = ""))
}





