library(tidyverse)

#load weather data
'pull in weather history'
weather                           <- read_csv('./csvs/Austin_weather_history.csv', na = "")
paste('nbr rows ',nrow(weather))
weather$salesdate                 <- as.Date(weather$DATE, format = "%Y-%m-%d")
'TMAX'
weather$TMAX                      <- as.numeric(weather$TMAX)
weather                           <- subset(weather,!is.na(weather$TMAX))
paste('nbr rows ',nrow(weather))
paste('nbr of na values',sum(is.na(weather$TMAX)))
paste('min',min(weather$TMAX),'max',max(weather$TMAX))
'TMIN'
weather$TMIN                      <- as.numeric(weather$TMIN)
weather                           <- subset(weather,!is.na(weather$TMIN))
paste('nbr rows ',nrow(weather))
paste('nbr of na values',sum(is.na(weather$TMIN)))
paste('min',min(weather$TMIN),'max',max(weather$TMIN))
'PRCP'
weather$PRCP                      <- as.numeric(weather$PRCP)
weather$PRCP[is.na(weather$PRCP)] <- 0
paste('nbr rows ',nrow(weather))
paste('nbr of na values',sum(is.na(weather$PRCP)))
paste('min',min(weather$PRCP),'max',max(weather$PRCP))
'SNOW'
weather$SNOW                      <- as.numeric(weather$SNOW)
weather$SNOW[is.na(weather$SNOW)] <- 0
paste('nbr rows ',nrow(weather))
paste('nbr of na values',sum(is.na(weather$SNOW)))
paste('min',min(weather$SNOW),'max',max(weather$SNOW))

#load sales data, with predictions and residuals
so_data = readRDS(file = "./dataframes/so_data_w_resid.rds")

#merge the two
so_data                           <- merge(weather,so_data,by="salesdate")
paste('rows after merging weather and so_data',nrow(so_data))
columns_to_drop                   <- c("STATION","NAME","DATE","quarterYear")
so_data                           <- so_data[ , !(names(so_data) %in% columns_to_drop)]
paste('rows after dropping unused columns: ',nrow(so_data))
write.csv(so_data,'./csvs/weather_and_residuals_added.csv', row.names=FALSE)
saveRDS(so_data, file = "./dataframes/weather_and_residuals_added.rds")
