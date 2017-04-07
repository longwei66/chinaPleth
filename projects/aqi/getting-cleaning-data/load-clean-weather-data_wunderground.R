## =============================================================================
##      Load and clean all weather data from wundeground
## =============================================================================

## -----------------------------------------------------------------------------
## Configuration
## -----------------------------------------------------------------------------
# libraries
library(data.table)
library(lubridate)

# source Folder
weatherFolder <- '../data/chinaPlethData/local/weather/wunderground/'


## -----------------------------------------------------------------------------
## Load Data
## -----------------------------------------------------------------------------
## list available weather files
weatherFiles <- list.files(weatherFolder)

## function to load weather as data.table
loadWeather <- function(fileName){
        data <- fread(input = paste(weatherFolder, fileName, sep = ""))
        return(data)
}

## load all files in a list of data.table
masterWeather <- rbindlist(lapply(weatherFiles, loadWeather))

## -----------------------------------------------------------------------------
## Clean Data
## -----------------------------------------------------------------------------

## Remove V1 column
masterWeather[ , V1 := NULL ]

## Remove NA values for City
masterWeather <- masterWeather[!is.na(City),]

## date.time
masterWeather[ , date.time := lubridate::ymd_hms(date.time)]

## date_obs
masterWeather[ , date_obs := as.Date(date.time)]

## remove Local.time
masterWeather[ , Local.Time := NULL]

## add weekday
masterWeather[ , weekday := weekdays(date.time)]

## add week-end
masterWeather[ , weekend := "no"]
masterWeather[ weekday %in% c("Sunday","Saturday"), weekend := "yes"]

## Temperature, update -9999 which are NA values
masterWeather[ Temperature == -9999, Temperature := NA ]

## Dewpoint, update -9999 which are NA values
masterWeather[ Dewpoint == -9999,  Dewpoint := NA ]

## Humidity, nothing to do

## Pressure, update -9999 which are NA values
masterWeather[ Pressure == -9999, Pressure:= NA]

## Visibility, nothing to do

## Wind.Direction.code, nothing to do

## Wind.Direction.degree, nothing to do

## Wind.Speed
# change calm to zero
masterWeather[ Wind.Speed == "Calm" , Wind.Speed := "0"]
# convert to numeric
masterWeather[ , Wind.Speed := as.numeric(Wind.Speed) ]

## Gust.Speed
# change empty data to NA values
masterWeather[ Gust.Speed %in% c("-","") , Gust.Speed := NA]
# convert to numeric
masterWeather[ , Gust.Speed := as.numeric(Gust.Speed) ]

## Precipitation (remove)
masterWeather[ , Precipitation := NULL ]

## Events
masterWeather[ Events == "" , Events := NA]

## Conditions
masterWeather[ Conditions %in% c("", "Unknown") , Conditions := NA]
