## Prepare the ECDC data so that I have for each country and each year the 
# resistance level for quinolones, the usage for quinolones and the 
# amount of data it is based on. 

#Plot the European CSC data
setwd("~/Documents/GitHub/CoexistencePaper/")
library(ggplot2)
library(ggrepel)
library(Polychrome)
library(here)
library(tidyverse)

#Read the data (downloaded from ECDC database https://atlas.ecdc.europa.eu/public/index.aspx)
ECDCResistance <- read.csv("ECDCData/ECDC_surveillance_data_Antimicrobial_resistance_complete_DownloadApril2024.csv", 
                           stringsAsFactors = FALSE)
#Make country name as factor
ECDCResistance$RegionName<-as.factor(ECDCResistance$RegionName)
##Make a column for the bacterium and a column for the drug. 
#The Population column has info on the bacterium and the drug, separated by a "|"
ECDCResistance$Bacterium<-str_split_i(ECDCResistance$Population,pattern ='\\|',1)
ECDCResistance$Drug<-str_split_i(ECDCResistance$Population,pattern ='\\|',2)
#Keep only E. coli
ECDCResistance <- ECDCResistance[ECDCResistance$Bacterium == "Escherichia coli",]
#Keep only "Fluoroquinolones"
ECDCResistance<- ECDCResistance[ECDCResistance$Drug %in% "Fluoroquinolones",]
#Make sure the value is read as a number (this is the % resistant) ignore warning (not related to E coli data)
ECDCResistance$NumValue <- as.numeric(as.character(ECDCResistance$NumValue)) 

#Keep the number of samples in a temporary dataframe. 
ECDCResistanceNumSamples <- ECDCResistance[ECDCResistance$Indicator == "Total tested isolates",]

#Keep only Resistance percentage info (not other indicators)
ECDCResistance <- ECDCResistance[ECDCResistance$Indicator == "R - resistant isolates, percentage  ",] ##only keep the % resistance indicator
ECDCResistance$SampleSize <- NA #Add the sample size to each row
for (i in 1:nrow(ECDCResistance)){
  ECDCResistance$SampleSize[i] <- ECDCResistanceNumSamples$NumValue[
    ECDCResistanceNumSamples$Time == ECDCResistance$Time[i] & ECDCResistanceNumSamples$RegionName == ECDCResistance$RegionName[i]]
}

ggplot(data = ECDCResistance, mapping = aes(x = Time, y = NumValue))+
  geom_line(aes(color = RegionName))+
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0,100))

#### READ in the data on the treatment levels, which I have in QuinoloneUsageOverTimeEurope.csv
Quin <- read.csv("ECDCData/QuinoloneUsageOverTimeEurope.csv", colClasses = "character")  # Read all as character
Quin[,2:13] <- lapply(Quin[,2:13] , function(x) as.numeric(gsub("[^0-9\\.]", "", x)))  # Remove non-numeric characters and convert

Quin_long <- pivot_longer(
  Quin,
  cols = 2:13, # Columns to pivot
  names_to = "Year",    # Name of the new column for years
  values_to = "Usage"   # Name of the new column for values
) 
Quin_long$Year <-  as.numeric(gsub("X", "", Quin_long$Year))

### Now, let's add the Quin resistance data to the same dataframe
Quin_long$ResistanceLevel <- NA
Quin_long$SampleSize <- NA

for (i in 1:nrow(Quin_long)){
  CountryName = Quin_long$Country.name[i]
  Year = Quin_long$Year[i]
  print(paste0(CountryName, Year))
  if (CountryName %in% ECDCResistance$RegionName){
  ResistanceLevel = ECDCResistance$NumValue[ECDCResistance$RegionName == CountryName & ECDCResistance$Time== Year] ## need single & here for element wise AND
  SampleSize = ECDCResistance$SampleSize[ECDCResistance$RegionName == CountryName & ECDCResistance$Time== Year]
  if (length(ResistanceLevel)==1) {
    Quin_long$ResistanceLevel[i] <- ResistanceLevel; print ("resistance level added")
    Quin_long$SampleSize[i] <- SampleSize}
  }
  if (!CountryName %in% ECDCResistance$RegionName){print(paste0 (CountryName, "  has no resistance data"))}
}

write.csv(Quin_long, file = "ECDCData/QuinoloneResistanceAndUsageEurope_Feb2025.csv", row.names = FALSE)



