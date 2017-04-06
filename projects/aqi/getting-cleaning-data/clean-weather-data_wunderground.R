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
