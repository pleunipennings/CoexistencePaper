
setwd("~/Documents/GitHub/CoexistencePaper/")
library(ggplot2)

Resistances = c("Ciprofloxacin_SIR" , "Cefotaxim_SIR" , "Gentamicin_SIR", "Ampicillin_SIR", "Piperacillin_tazobactam_SIR")
###Start with Cipro

####
## Here we are reading files we created earlier using Norway_Clusters_Tree_4Drugs.R
###

DrugResistance = Resistances[1]
ClusterSizeDFCiprofloxacin = read.csv(file = paste0("NorwayData/CLusterSizeDF_",DrugResistance,".csv"))
length(which(ClusterSizeDFCiprofloxacin$clustersize>1)) / length(which(ClusterSizeDFCiprofloxacin$clustersize>0))
ClusterSizeDFCiprofloxacin$DrugResistance <- "Ciprofloxacin"

DrugResistance = Resistances[2]
ClusterSizeDFCefotaxim = read.csv(file = paste0("NorwayData/CLusterSizeDF_",DrugResistance,".csv"))
length(which(ClusterSizeDFCefotaxim$clustersize>1)) / length(which(ClusterSizeDFCefotaxim$clustersize>0))
ClusterSizeDFCefotaxim$DrugResistance <- "Cefotaxim"

DrugResistance = Resistances[3]
ClusterSizeDFGentamicin = read.csv(file = paste0("NorwayData/CLusterSizeDF_",DrugResistance,".csv"))
length(which(ClusterSizeDFGentamicin$clustersize>1)) / length(which(ClusterSizeDFGentamicin$clustersize>0))
ClusterSizeDFGentamicin$DrugResistance <- "Gentamicin"

DrugResistance = Resistances[4]
ClusterSizeDFAmpicillin = read.csv(file = paste0("NorwayData/CLusterSizeDF_",DrugResistance,".csv"))
length(which(ClusterSizeDFAmpicillin$clustersize>1)) / length(which(ClusterSizeDFAmpicillin$clustersize>0))
ClusterSizeDFAmpicillin$DrugResistance <- "Ampicillin"

DrugResistance = Resistances[5]
ClusterSizeDFPipTaz = read.csv(file = paste0("NorwayData/CLusterSizeDF_",DrugResistance,".csv"))
length(which(ClusterSizeDFPipTaz$clustersize>1)) / length(which(ClusterSizeDFPipTaz$clustersize>0))
ClusterSizeDFPipTaz$DrugResistance <- "PipTaz"

ClusterSizeDF <- rbind(ClusterSizeDFCiprofloxacin, ClusterSizeDFCefotaxim, ClusterSizeDFGentamicin, ClusterSizeDFAmpicillin, ClusterSizeDFPipTaz)

head(ClusterSizeDF)

##Create an overview data frame with the survival time of clusters (ie how many years they are seen in the data)

OverviewAll  <- ClusterSizeDF %>%
  group_by(CLusterSurvivalTime, DrugResistance) %>%
  summarize(SumClusters = sum(clustersize), NClusters = n(), .groups = 'drop')
OverviewAll <- as.data.frame(OverviewAll)
OverviewAll$CLusterSurvivalTime123<-OverviewAll$CLusterSurvivalTime
OverviewAll$CLusterSurvivalTime123[OverviewAll$CLusterSurvivalTime123>8]<-">8 years"
OverviewAll$CLusterSurvivalTime123[OverviewAll$CLusterSurvivalTime123>3]<-"4-8 years"
OverviewAll$CLusterSurvivalTime123[OverviewAll$CLusterSurvivalTime123==3]<-"3 years"
OverviewAll$CLusterSurvivalTime123[OverviewAll$CLusterSurvivalTime123==2]<-"2 years"
OverviewAll$CLusterSurvivalTime123[OverviewAll$CLusterSurvivalTime123==1]<-"1 year"
OverviewAll$CLusterSurvivalTime123<-factor(OverviewAll$CLusterSurvivalTime123, levels = c(">8 years","4-8 years", "3 years", "2 years", "1 year"))
OverviewAll$CLusterSurvivalTime<-factor(OverviewAll$CLusterSurvivalTime, levels = 16:1)

###########################
#####
#Figure 3C
#####
###########################

png(paste0("Figure_3C_Norway_LongevityClusters.png"), width = 5.5, height = 5.5, units = "in", res = 200)

ggplot(data = OverviewAll, aes(x = DrugResistance, y = NClusters, fill = CLusterSurvivalTime)) +
  geom_bar(position = "fill",stat = "identity", color = "black")+
  ylab("Longevity of resistance clusters")+
  xlab("")+
  #scale_fill_brewer(palette = "Set1")+
  theme_bw()+
  scale_fill_discrete(name = "Longevity", labels = paste0(c(16,14:1), " y"))+
  ggtitle("Longevity of resistance clusters")+
  theme(legend.position="right")
dev.off()

###########################
#####
#Figure 3B
#####
###########################

OverviewTotalClusters <- 
  OverviewAll %>% group_by(DrugResistance)%>%
  summarize(NClusters = sum(NClusters), .groups = 'drop')

color_hex <- c( 'darkgrey','#000075','#e6194b', '#008080', '#e6beff')
color_hex <- c( "#00b0f6", "#00bf7d", "#ff67a4", "#a3a500", "#e58700")
names(color_hex) = OverviewTotalClusters$DrugResistance
OverviewTotalClusters$DrugResistance<-as.factor(OverviewTotalClusters$DrugResistance)

png(paste0("Figure_3B_Norway_NumClusters.png"), width = 5.5, height = 5.5, units = "in", res = 200)
ggplot(data = OverviewTotalClusters, aes(x = DrugResistance, y = NClusters,fill = DrugResistance))+
  geom_bar(stat="identity", show.legend = FALSE, color = "black")+
  ylab("Number of origins of resistance")+
  scale_fill_manual(values = color_hex)+
  #scale_fill_brewer(palette = "Accent")+
  xlab("")+
  theme_bw()+
  geom_text(aes(label=NClusters), position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Number of origins of resistance phenotype")
dev.off()


##############################################
###
#Getting and wrangling the resistance data from the Gladstone dataset
###
##############################################

NorwayMetaData <- read.csv("NorwayData/NorwayMetaData.csv") #this file was called "data.csv" in the Gladstone paper
NorwaySummary = data.frame(year = sort(unique(NorwayMetaData$year)), Gentamicin_RES = 0, 
                           Ciprofloxacin_RES =0,  Cefotaxim_RES = 0, Ampicillin_RES = 0, PipTaz_RES = 0) #Create columns for % resistance
for (i in 1:nrow(NorwaySummary)){  #Go through each year and get the % resistant samples for that year and that drug
  y = NorwaySummary$year[i]
  NorwaySummary$Gentamicin_RES[i] = 
    length(which(NorwayMetaData$Gentamicin_SIR=="R"& NorwayMetaData$year==y))/length(which(!is.na(NorwayMetaData$Gentamicin_SIR) & NorwayMetaData$year==y))
  NorwaySummary$Ciprofloxacin_RES[i] = 
    length(which(NorwayMetaData$Ciprofloxacin_SIR=="R"& NorwayMetaData$year==y))/length(which(!is.na(NorwayMetaData$Ciprofloxacin_SIR) & NorwayMetaData$year==y))
  NorwaySummary$Cefotaxim_RES[i] = 
    length(which(NorwayMetaData$Cefotaxim_SIR=="R"& NorwayMetaData$year==y))/length(which(!is.na(NorwayMetaData$Cefotaxim_SIR) & NorwayMetaData$year==y))
  NorwaySummary$Ampicillin_RES[i] = 
    length(which(NorwayMetaData$Ampicillin_SIR=="R"& NorwayMetaData$year==y))/length(which(!is.na(NorwayMetaData$Ampicillin_SIR) & NorwayMetaData$year==y))
  NorwaySummary$PipTaz_RES[i] = 
    length(which(NorwayMetaData$Piperacillin_tazobactam_SIR=="R"& NorwayMetaData$year==y))/length(which(!is.na(NorwayMetaData$Piperacillin_tazobactam_SIR) & NorwayMetaData$year==y))
}

#Read the data (downloaded from ECDC database https://atlas.ecdc.europa.eu/public/index.aspx)
ECDCResistanceAllDrugs <- read.csv("ECDC_surveillance_data_Antimicrobial_resistance_complete_DownloadApril2024.csv", 
                                   stringsAsFactors = FALSE)
#Keep only Resistance percentage info (not other indicators)
ECDCResistanceAllDrugs <- ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Indicator == "R - resistant isolates, percentage  ",] ##only keep the % resistance indicator
#Make sure the value is read as a number (this is the % resistant) ignore warning (not related to E coli data)
ECDCResistanceAllDrugs$NumValue <- as.numeric(as.character(ECDCResistanceAllDrugs$NumValue)) 
#Make country name as factor
ECDCResistanceAllDrugs$RegionName<-as.factor(ECDCResistanceAllDrugs$RegionName)
##Make a column for the bacterium and a column for the drug. 
#The Population column has info on the bacterium and the drug, separated by a "|"
ECDCResistanceAllDrugs$Bacterium<-str_split_i(ECDCResistanceAllDrugs$Population,pattern ='\\|',1)
ECDCResistanceAllDrugs$Drug<-str_split_i(ECDCResistanceAllDrugs$Population,pattern ='\\|',2)
#Keep only E. coli
ECDCResistanceAllDrugs <- ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Bacterium == "Escherichia coli",]
#Keep only 4 drugs
DrugsToKeep <- c("Aminoglycosides", "Aminopenicillins", "Fluoroquinolones", "Third-generation cephalosporins")
ECDCResistanceAllDrugs<- ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Drug %in% DrugsToKeep,]

ECDCResistanceAllDrugs<- ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$RegionName == "Norway",]

##############################################
###
# Making figures that show % resistance over time in 6 countries plus Norway plus the Gladstone data
# Figure 3A 
###
##############################################

##Aminoglycosides (including Gentamicin)
png("Figure_3A_Resistance_Trends_Gladstone.png", width = 5.5, height = 5.5, units = "in", res = 300)
ggplot(data = NorwaySummary, 
       mapping = aes(x = year, y = Gentamicin_RES))+
  #geom_line()+
  scale_y_continuous(limits = c(0,80))+
  scale_x_continuous(limits = c(2000, 2023))+
  theme_bw()+
  labs(y = "% drug resistance")+
  theme(legend.position = "none")+
  ggtitle("Resistance trends in Norway")+
  geom_line(data = NorwaySummary, aes(x = as.numeric(as.character(year)), y = Ampicillin_RES*100), col = color_hex[1])+
  geom_line(data = ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Drug == "Aminopenicillins",],aes(x = Time, y = NumValue), col = color_hex[1], linetype = "dashed")+ 
  geom_text(x = 2022, y = 42, label = "Ampicillin", col = color_hex[1])+
  
  geom_line(data = NorwaySummary, aes(x = as.numeric(as.character(year)), y = Cefotaxim_RES*100), col = color_hex[2])+
  geom_line(data = ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Drug == "Third-generation cephalosporins",],aes(x = Time, y = NumValue), col = color_hex[2], linetype = "dashed")+ 
  geom_text(x = 2022, y = 7.5, label = "Cefotaxim", col = color_hex[2])+

  geom_line(data = NorwaySummary, aes(x = as.numeric(as.character(year)), y = Ciprofloxacin_RES*100), col = color_hex[3])+
  geom_line(data = ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Drug == "Fluoroquinolones",],aes(x = Time, y = NumValue), col = color_hex[3], linetype = "dashed")+ 
  geom_text(x = 2022, y = 13, label = "Ciprofloxacin", col = color_hex[3])+

  geom_line(data = NorwaySummary, aes(x = as.numeric(as.character(year)), y = Gentamicin_RES*100),col = color_hex[4])+
  geom_line(data = ECDCResistanceAllDrugs[ECDCResistanceAllDrugs$Drug == "Aminoglycosides",],aes(x = Time, y = NumValue), linetype = "dashed",col = color_hex[4])+ 
  geom_text(x = 2022, y = 3.5, label = "Gentamicin", col = color_hex[4])+
  
  geom_line(data = NorwaySummary, aes(x = as.numeric(as.character(year)), y = PipTaz_RES*100), col = color_hex[5])+
  geom_text(x = 2017.7, y = 2, label = "PipTaz", col = color_hex[5])
dev.off()


