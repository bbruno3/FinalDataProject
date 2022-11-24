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

## Create a new column called DATE that is an actual date in R
# note that R uses a 0 based index for dates 
# example: 1-1-2022 is day 0 of the year, AZMET has it as 1 so subtract 1 from DOY
AZMET$DATE <- as.Date(AZMET$DOY-1, origin = paste(AZMET$Year, "-01-01", sep = ""))

## if you need to write out a CSV file to save your work
# you can use this command, it saves the rownames so the
# file will make sense on its own
write.csv(AZMET, "AZMET.csv", row.names = TRUE)

##### Begin Your Project Code Here #####

head(AZMET)
AZMET_Filenames
