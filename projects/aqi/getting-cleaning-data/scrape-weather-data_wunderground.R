## =============================================================================
##      Scrape Weather hourly data from wunderground
##      For the cities : Shanghai, Beijing, Chengdu, Guangzhou, Shenyang, Paris
## =============================================================================

## Needed libraries and system configuration
## -----------------------------------------
library(XML)
library("rvest")
library("dplyr")
library("lubridate")
Sys.setlocale(category = "LC_ALL", locale = "C")

## Data Configuration
## -----------------------------------------

## Cities : city names and url to get weather
uriCityWeather <- read.csv(file = "./data/share/aqi/wunderground-cities-data-source-url.csv", 
                           sep = ";", 
                           header = TRUE)
## Plan number of days per year
## nbDay <- 365

## Plan for data collection per year and city
listOfCityYear <- data.frame(
        year = c(
                #"2016","2016","2016","2016","2016","2016"#,
                #"2015","2015","2015","2015","2015","2015"#,
                #"2014","2014","2014","2014","2014","2014"#,
                #"2013","2013","2013","2013","2013","2013"
                ),
        city = c(
                #"Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris"#,
                #"Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris"#,
                #"Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris"#,
                #"Shanghai","Beijing","Chengdu","Guangzhou","Shenyang","Paris"
                 )
)



##      Scrap the data
## -----------------------------------------

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

        nbDay <- as.numeric(as.Date(paste(year,"-12-31", sep = "")) - as.Date(paste(year,"-01-01", sep = "")))
        
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
}

