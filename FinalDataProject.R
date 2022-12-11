# BE310 Fall 2022
# Final Project
# Barbara Bruno

# load required libraries
library(readr)
library(openxlsx)
library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(here)
library(ggplot2)

## Read In Data Files from AZMET
# Use this header record for AZMET daily data
# Year,DOY,SatationNumber,AirTMax,AirTMin,AirTMean,RHMax,RHMin,RHMean,VPDMean,SolarRadTotal,PrecipTotal,SoilTempMax_4in,SoilTempMin_4in,SoilTempMean_4in,SoilTempMax_20in,SoilTempMin_20in,SoilTempMean_20in,WindSpeedMean,WindVecMag_day,WindVecDir_day,WindDirSTD_day,WindSpeedMax,HeatUnits,ETo,RefETo,ActuaVPMean_day,DewpoinMean_day
#"Year","DOY","SatationNumber","AirTMax","AirTMin","AirTMean","RHMax","RHMin","RHMean","VPDMean","SolarRadTotal","PrecipTotal","SoilTempMax_4in","SoilTempMin_4in","SoilTempMean_4in","SoilTempMax_20in","SoilTempMin_20in","SoilTempMean_20in","WindSpeedMean","WindVecMag_day","WindVecDir_day","WindDirSTD_day","WindSpeedMax","HeatUnits","ETo","RefETo","ActuaVPMean_day","DewpoinMean_day"

## This will load ALL .csv files that are contained in the local "data/" directory
# it assumes that any .csv in that directory is an AZMET Daily Data file
# **AZMET Daily Data files from 2003 to present** work with this configuration
AZMET_Filenames <- list.files(path = here("data"), full.names = TRUE, pattern = "\\.csv$")

## Read in the CSV files and assign column names.
AZMET <- read_csv(file = AZMET_Filenames, 
                  col_names=c("Year","DOY","StationNumber","AirTMax","AirTMin","AirTMean","RHMax","RHMin","RHMean","VPDMean","SolarRadTotal","PrecipTotal","SoilTempMax_4in","SoilTempMin_4in","SoilTempMean_4in","SoilTempMax_20in","SoilTempMin_20in","SoilTempMean_20in","WindSpeedMean","WindVecMag_day","WindVecDir_day","WindDirSTD_day","WindSpeedMax","HeatUnits","ETo","RefETo","ActuaVPMean_day","DewpoinMean_day"),
                  show_col_types = FALSE)

#Can run after you Run the AZMET code above to look at AZMET Data.
AZMET

## Create a new column called DATE that is an actual date in R
# note that R uses a 0 based index for dates 
# example: 1-1-2022 is day 0 of the year, AZMET has it as 1 so subtract 1 from DOY
AZMET$DATE <- as.Date(AZMET$DOY-1, origin = paste(AZMET$Year, "-01-01", sep = ""))
AZMET

## load station id, name table as a cross-reference
# specify column names that match other data
Stations <- read_csv(file = here("stations.csv"), 
         col_names=c("StationNumber","StationName"),
         skip = 1,
         show_col_types = FALSE)


AZMET_Filenames
# add a month column for summarizing
AZMET$Month <- lubridate::month(AZMET$DATE)
# add the station name based on the station number using a join
AZMET <- left_join(AZMET, Stations, by = "StationNumber")
# this should not be continuout
AZMET$StationNumber <- as.factor(AZMET$StationNumber)

######
# build monthly value summaries
monthly.tmp <- AZMET[,c("Month", "StationName", "AirTMax", "PrecipTotal", "SolarRadTotal","WindSpeedMax", "RHMax","ETo","DewpoinMean_day" )] %>%
  group_by(StationName, Month) %>%
  mutate(Temp_month = mean(AirTMax, na.rm = TRUE)) %>%
  mutate(Precip_month = mean(PrecipTotal, na.rm = TRUE)) %>%
  mutate(SolarRadTotal_month = mean(SolarRadTotal, na.rm = TRUE)) %>%
  mutate(WindSpeedMax_month = mean(WindSpeedMax, na.rm = TRUE)) %>%
  mutate(RHMax_month = mean(RHMax, na.rm = TRUE)) %>%
  mutate(ETo_month = mean(ETo, na.rm = TRUE)) %>%
  mutate(DewpoinMean_day_month = mean(DewpoinMean_day, na.rm = TRUE)) %>%
  ungroup()
# remove all but monthly summary
monthly.tmp <- monthly.tmp[,-c(3:9)]
# remove duplicates
monthly.tmp <- monthly.tmp %>% distinct()
######

# plot all temperature values by month
monthly.tmp %>%
  ggplot( aes(x=Month, y=Temp_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Temperature for Five Years", y = "Average Temperature (°C)", x = "Month") +
  geom_line()

# plot all precipitation values by month and station
monthly.tmp %>%
  ggplot( aes(x=Month, y=Precip_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Precipitation for Five Years", y = "Average Precipitation (mm)", x = "Month") +
  geom_line()

# plot all solar radiation valued by month and station
monthly.tmp %>%
  ggplot( aes(x=Month, y=SolarRadTotal_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Solar Radiation for Five Years", y = "Average Solar Radiation (mJ/m²)", x = "Month") +
  geom_line()

# plot all relative humidity max within a day
monthly.tmp %>%
  ggplot( aes(x=Month, y=RHMax_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Relative Humidity for Five Years", y = "Average Relative Humidity (%)", x = "Month") +
  geom_line()

#Here are examples of the codes flexibility. Was not used for paper 

# plot all max wind speed in mph within a day
monthly.tmp %>%
  ggplot( aes(x=Month, y=WindSpeedMax_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Wind Speed per Month for Five Years", y = "Average Wind Speed (mph)", x = "Month") +
  geom_line()

# plot all reference evapotranspiration in inches 
monthly.tmp %>%
  ggplot( aes(x=Month, y=ETo_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Evapotranspiration for Five Years", y = "Average Relative Humidity (mm)", x = "Month") +
  geom_line()

# plot dewpoint 
monthly.tmp %>%
  ggplot( aes(x=Month, y=DewpoinMean_day_month, group=StationName, color=StationName)) +
  labs(title = "Average Seasonal Dew Point for Five Years", y = "Average Dew Point (°C Td)", x = "Month") +
  geom_line()

######

#Second Graph 

# build monthly value summaries
monthly.tmp <- AZMET[,c("Month", "StationName", "YM", "AirTMax", "PrecipTotal", "SolarRadTotal","WindSpeedMax", "RHMax","ETo","DewpoinMean_day" )] %>%
  group_by(StationName, YM) %>%
  mutate(Temp_month = mean(AirTMax, na.rm = TRUE)) %>%
  mutate(Precip_month = mean(PrecipTotal, na.rm = TRUE)) %>%
  mutate(SolarRadTotal_month = mean(SolarRadTotal, na.rm = TRUE)) %>%
  mutate(WindSpeedMax_month = mean(WindSpeedMax, na.rm = TRUE)) %>%
  mutate(RHMax_month = mean(RHMax, na.rm = TRUE)) %>%
  mutate(ETo_month = mean(ETo, na.rm = TRUE)) %>%
  mutate(DewpoinMean_day_month = mean(DewpoinMean_day, na.rm = TRUE)) %>%
  ungroup()
# remove all but monthly summary
monthly.tmp <- monthly.tmp[,-c(4:10)]
# remove duplicates
monthly.tmp <- monthly.tmp %>% distinct()

######

# plot all temperature values by month
monthly.tmp %>%
  ggplot( aes(x=YM, y=Temp_month,  color=StationName)) +
  labs(title = "Monthly Average for Five Years of Temperature", y = "Average Temperature per Month (°C)", x = "Month") +
  geom_line()

# plot all precipitation values by month and station
monthly.tmp %>%
  ggplot( aes(x=YM, y=Precip_month, color=StationName)) +
  labs(title = "Monthly Average for Five Years of Precipitation", y = "Average Precipitation per Month (mm)", x = "Month") +
  geom_line()

# plot all solar radiation valued by month and station
monthly.tmp %>%
  ggplot( aes(x=YM, y=SolarRadTotal_month, color=StationName)) +
  labs(title = "Monthly Average for Five Years of Solar Radiation", y = "Average Solar Radiation per Month (mJ/m²)", x = "Month") +
  geom_line()

# plot all relative humidity max within a day
monthly.tmp %>%
  ggplot( aes(x=YM, y=RHMax_month,  color=StationName)) +
  labs(title = "Monthly Average for Five Years of Relative Humidity", y = "Average Relative Humidity per Month (%)", x = "Month") +
  geom_line()

#Here are examples of the codes flexibility. Was not used for as part off my data 

# plot all reference evapotranspiration in inches 
monthly.tmp %>%
  ggplot( aes(x=YM, y=ETo_month, color=StationName)) +
  labs(title = "Monthly Average for Five Years of Evapotranspiration", y = "Average Evapotranspiration per Month (mm)", x = "Month") +
  geom_line()

# plot dewpoint 
monthly.tmp %>%
  ggplot( aes(x=YM, y=DewpoinMean_day_month,  color=StationName)) +
  labs(title = "Monthly Average for Five Years of Dew Point", y = "Average Dew Point per Month (°C Td)", x = "Month") +
  geom_line()

# plot all max wind speed in mph within a day
monthly.tmp %>%
  ggplot( aes(x=YM, y=WindSpeedMax_month, color=StationName)) +
  labs(title = "Monthly Average for Five Years of Wind Speed", y = "Average Wind Speed per Month (mph)", x = "Month") +
  geom_line()
 