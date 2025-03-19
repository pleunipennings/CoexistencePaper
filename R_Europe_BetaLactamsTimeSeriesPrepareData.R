## Prepare the ECDC data so that I have for each country and each year the 
# resistance level for BETALACTAMS for S AUREUS (not for the paper but for book chapter with meike wittmann), the usage for quinolones and the 
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
#Keep only "Staphylococcus aureus"
ECDCResistance <- ECDCResistance[ECDCResistance$Bacterium == "Staphylococcus aureus",]
#Keep only ""Meticillin (MRSA)""
ECDCResistance<- ECDCResistance[ECDCResistance$Drug %in% "Meticillin (MRSA)",]
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
BetaLactam <- read.csv("ECDCData/BetaLactamUsageOverTimeEurope.csv", colClasses = "character")  # Read all as character
BetaLactam[,2:13] <- lapply(BetaLactam[,2:13] , function(x) as.numeric(gsub("[^0-9\\.]", "", x)))  # Remove non-numeric characters and convert

BetaLactam_long <- pivot_longer(
  BetaLactam,
  cols = 2:13, # Columns to pivot
  names_to = "Year",    # Name of the new column for years
  values_to = "Usage"   # Name of the new column for values
) 
BetaLactam_long$Year <-  as.numeric(gsub("X", "", BetaLactam_long$Year))

### Now, let's add the BetaLactam_long resistance data to the same dataframe
BetaLactam_long$ResistanceLevel <- NA
BetaLactam_long$SampleSize <- NA

for (i in 1:nrow(BetaLactam_long)){
  CountryName = BetaLactam_long$Country.name[i]
  Year = BetaLactam_long$Year[i]
  print(paste0(CountryName, Year))
  if (CountryName %in% ECDCResistance$RegionName){
  ResistanceLevel = ECDCResistance$NumValue[ECDCResistance$RegionName == CountryName & ECDCResistance$Time== Year] ## need single & here for element wise AND
  SampleSize = ECDCResistance$SampleSize[ECDCResistance$RegionName == CountryName & ECDCResistance$Time== Year]
  if (length(ResistanceLevel)==1) {
    BetaLactam_long$ResistanceLevel[i] <- ResistanceLevel; print ("resistance level added")
    BetaLactam_long$SampleSize[i] <- SampleSize}
  }
  if (!CountryName %in% ECDCResistance$RegionName){print(paste0 (CountryName, "  has no resistance data"))}
}

write.csv(BetaLactam_long, file = "ECDCData/BetaLactamSAureusResistanceAndUsageEurope_Feb2025.csv", row.names = FALSE)



