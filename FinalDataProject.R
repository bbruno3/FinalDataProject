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
                  col_names=c("Year","DOY","SatationNumber","AirTMax","AirTMin","AirTMean","RHMax","RHMin","RHMean","VPDMean","SolarRadTotal","PrecipTotal","SoilTempMax_4in","SoilTempMin_4in","SoilTempMean_4in","SoilTempMax_20in","SoilTempMin_20in","SoilTempMean_20in","WindSpeedMean","WindVecMag_day","WindVecDir_day","WindDirSTD_day","WindSpeedMax","HeatUnits","ETo","RefETo","ActuaVPMean_day","DewpoinMean_day"),
                  show_col_types = FALSE)

#Can run after you Run the AZMET code above to look at AZMET Data.
AZMET

## Create a new column called DATE that is an actual date in R
# note that R uses a 0 based index for dates 
# example: 1-1-2022 is day 0 of the year, AZMET has it as 1 so subtract 1 from DOY
AZMET$DATE <- as.Date(AZMET$DOY-1, origin = paste(AZMET$Year, "-01-01", sep = ""))
AZMET
## if you need to write out a CSV file to save your work
# you can use this command, it saves the rownames so the
# file will make sense on its own
#write.csv(df, "my_file_name.csv", row.names = TRUE)

##### Begin Your Analysis Code Here #####

AZMET_Filenames

#Test 1: simple bar graph
ggplot(data = pressure) +
  geom_col(mapping = aes(x = temperature, y = pressure))

#Test 2: simple bar graph
ggplot(data=AZMET[1:50,], aes(x=DATE, y=AirTMax, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=AZMET, aes(x=DATE, y=AirTMax, group= SatationNumber )) +
  geom_line()+
  geom_point()

  geom_col(mapping = aes(x = "DATE" , y = "AirTMax" ))

ggplot(AZMET_Filenames = "PrecipTotal") +
  geom_col(mapping = aes(x = "AirTMax" , y = "PrecipTotal" ))

#Test 3: Scatter plot
AZMET_Filenames %>% 
  group_by(Date) %>% 
  summarise(mean_Year = mean("Year"), mean_PrecipTotal = mean("PrecipTotal")) %>% 
  ggplot() +
  geom_point(mapping = aes(x = mean_Year, y = mean_PrecipTotal))

