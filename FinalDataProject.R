#configure github
#git config --global user.email bruno3@arizona.edu
#git config --global user.name bbruno3

#libraries
library(readr)
library(openxlsx)
library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)

#Files read in and assign column names
AZMET <- read.csv(file = AZMET_Filenames,
                  col_names=c("Year","DOY","SatationNumber","AirTMax","AirTMin","AirTMean","RHMax","RHMin","RHMean","VPDMean","SolarRadTotal","PrecipTotal","SoilTempMax_4in","SoilTempMin_4in","SoilTempMean_4in","SoilTempMax_20in","SoilTempMin_20in","SoilTempMean_20in","WindSpeedMean","WindVecMag_day","WindVecDir_day","WindDirSTD_day","WindSpeedMax","HeatUnits","ETo","RefETo","ActuaVPMean_day","DewpoinMean_day"),
                  show_col_types = FALSE) 
#New column call "DATE" that is the R date.
AZMET$DATE<- as.Date(AZMET$DOY-1, origin = paste(AZMET$Year, "-01-01", sep = ""))

#Save rownames
write.csv(df, "my_file_name.csv", row.names = TRUE)
