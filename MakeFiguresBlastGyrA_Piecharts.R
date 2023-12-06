setwd("~/Documents/Selection-Selection-Balance-Model")
#library(tidyverse)
library(dplyr)
library(ggplot2)
library(seqinr)
library("cowplot") 
library("grid") 

source("Rscripts_Simulations_Coexistence/ColorsSeqTypes.R")

UKMetaData <- read.csv(file = "Enterobase/UKMetaData_withBlastResults.csv")

####FRAC RES 20 STs 
FracRes<-UKMetaData %>%
  group_by(Collection.Year, ST_20) %>%
  summarise(Frac = sum(GyrA_AA83 =="L", na.rm = TRUE), 
            Num83L = sum(GyrA_AA83 =="L", na.rm = TRUE),
            NumObs = sum(GyrA_AA83 %in% c("L","S")), .groups = "keep") 
FracRes$Frac = FracRes$Frac/FracRes$NumObs

FracRes <- as.data.frame(FracRes)
years = 2008 : 2022
FracRes <- FracRes[FracRes$Collection.Year %in% years,]

#png(file = "Figures_EnteroBase/Bergjes_Oct30_2023.png", width = 780, height = 580, units = "px", pointsize = 12 )
ggplot(data = FracRes[FracRes$ST_20!="Other",], aes(Collection.Year, Frac))+
  geom_area(aes(Collection.Year, Frac, fill = as.factor(ST_20)))+
  facet_wrap(~ST_20, ncol = 4, scales="free")+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Fraction 83L mutant among strains of the ST")
#dev.off()

FracRes100<-UKMetaData %>%
  group_by(Collection.Year, ST_100) %>%
  summarise(Frac = sum(GyrA_AA83 =="L", na.rm = TRUE), 
            Num83L = sum(GyrA_AA83 =="L", na.rm = TRUE),
            NumObs = sum(GyrA_AA83 %in% c("L","S")), .groups = "keep") 
FracRes100$Frac = FracRes100$Frac/FracRes100$NumObs

FracRes100 <- as.data.frame(FracRes100)
years = 2008 : 2022
FracRes100 <- FracRes100[FracRes100$Collection.Year %in% years,]
FracRes100 <- FracRes100[!is.na(FracRes100$ST_100),]

FracRes100$FracOfAll <- NA
for (y in years){
  TotalS = length(which(UKMetaData$GyrA_AA83[UKMetaData$Collection.Year == y]=="S"))
  TotalL = length(which(UKMetaData$GyrA_AA83[UKMetaData$Collection.Year == y]=="L"))
  FracRes100$FracOfAll[FracRes100$Collection.Year==y] <- FracRes100$Num83L[FracRes100$Collection.Year==y]/(TotalL+TotalS)
}

OrderSTsST_100 <- names(sort(table(UKMetaData$ST_100), decreasing = TRUE))
FracRes100$ST_100<-factor(FracRes100$ST_100, levels = OrderSTsST_100)

df_Totals<-data.frame(Years = years, Num = rep(1,length(years)))
top_group_for_legend <- OrderSTsST_100[1:10]

#png(filename = "Figures_EnteroBase/FracRes100.png", width = 8, height = 4, units = "in", res = 300)
#set.seed(seed = 30)
#mycolorsGyr <- sample(colorRampPalette(brewer.pal(8, "Set3"))(101))
#names(mycolorsGyr) <- OrderSTsST_100

png(file = "Enterobase/Figures_EnteroBase/GyrAResults_Black.png", width = 9, height = 3, units = "in", res = 300 )
ggplot(data = FracRes100, aes(Collection.Year, FracOfAll))+
  geom_col(data = df_Totals, aes(Years, Num), fill = "646464")+
  #geom_col(data = FracRes100, aes(Collection.Year, FracOfAll, fill = ST_100))+
  geom_col(aes(Collection.Year, FracOfAll), fill = "black")+
  theme_bw()+
  theme(legend.position = "right")+
  ylab("Fraction with resistance")+
  scale_y_continuous(limits = c(0,1))+
  #  scale_fill_manual(values = sample(getPalette(colourCount)))+
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend) +
  labs(fill = "Common ST") 
dev.off()

FracRes100$label_vector = as.character(FracRes100$ST_100)
FracRes100$label_vector[FracRes100$FracOfAll<0.007]<-""
FracRes100$color <- mycolorsST_100[FracRes100$ST_100]
FracRes100$FracOfAll[is.na(FracRes100$FracOfAll)]<-0

png(file = "Enterobase/Figures_EnteroBase/ResultsGyraseA_PieCharts.png", width = 8, height = 3.5, units = "in", res = 300 )
par(mar = c(1, 1, 1, 1)) # Set the margin on all sides to 2
par(mfrow = c(1, 4))
for (yeartoPie in c(2009, 2012, 2018, 2021)){ 
#for (yeartoPie in years){
  #genetoPie = "blaCTX.M.15_1"
  pie(FracRes100$FracOfAll[FracRes100$Collection.Year==yeartoPie], 
      col = FracRes100$color[FracRes100$Collection.Year==yeartoPie],
      labels = FracRes100$label_vector[FracRes100$Collection.Year==yeartoPie], 
      main = paste(yeartoPie)
  )
}
dev.off()




FracResAllSTTogether<-UKMetaData %>%
  group_by(Collection.Year) %>%
  summarise(Frac = sum(GyrA_AA83 =="L", na.rm = TRUE), 
            Num83L = sum(GyrA_AA83 =="L", na.rm = TRUE),
            NumObs = sum(GyrA_AA83 %in% c("L","S"))) 
FracResAllSTTogether$Frac = FracResAllSTTogether$Frac/FracResAllSTTogether$NumObs
FracResAllSTTogether<-FracResAllSTTogether[FracResAllSTTogether$Collection.Year %in% years,]
FracResAllSTTogether$FracSus = 1- FracResAllSTTogether$Frac

png(file = "Figures_EnteroBase/AllSTsTogether.png", width = 780, height = 580, units = "px", pointsize = 12 )
ggplot(data = FracResAllSTTogether, aes(Collection.Year, Frac))+
  geom_line(aes(Collection.Year, Frac), linewidth = 2)+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Fraction 83L mutant among all UK isolates")+
  scale_y_continuous(limits = c(0,1))
dev.off()

TenSTs <- tail(names(sort(table(UKMetaData$ST_100))),20)

ggplot(data = FracRes100[FracRes100$Collection.Year %in% years & FracRes100$ST_100 %in% TenSTs,], aes(Collection.Year, Frac))+
  geom_col(aes(Collection.Year, Frac, fill = as.factor(ST_100)))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Fraction S83L")+
  scale_fill_manual(values = mycolorsGyr) +
  scale_y_continuous(limits = c(0,1))+
  facet_wrap("ST_100")

png(file = "Figures_EnteroBase/AbricateResultsGyrA_TwoSTs.png", width = 4, height = 2, units = "in", res = 300 )
TwoSTs <- c(69, 29)
ggplot(data = FracRes100[FracRes100$Collection.Year %in% years & FracRes100$ST_100 %in% TwoSTs,], aes(Collection.Year, Frac))+
  geom_area(aes(Collection.Year, Frac, fill = as.factor(ST_100)))+
  theme_classic()+
  theme(legend.position = "none")+
  ylab(paste("Fraction S83L"))+
  scale_fill_manual(values = mycolorsGyr[which(names(mycolorsGyr) %in% TwoSTs)]) +
  facet_wrap("ST_100")
dev.off()

#Unrelated To resistance. How many samples do we have from each ST? 
FracRes100$FracST <- NA
for (y in years){
  Total = sum(FracRes100$NumObs[FracRes100$Collection.Year==y])
  FracRes100$FracST[FracRes100$Collection.Year==y] <- FracRes100$NumObs[FracRes100$Collection.Year==y]/Total
}

png(file = "Figures_EnteroBase/AllSTsByYear.png", width = 780, height = 580, units = "px", pointsize = 12 )
ggplot(data = FracRes100, aes(Collection.Year, FracST))+
  #geom_col(data = FracRes100, aes(Collection.Year, FracOfAll, fill = ST_100))+
  geom_col(aes(Collection.Year, FracST, fill = ST_100))+
  theme_bw()+
  theme(legend.position = "right")+
  ylab("Fraction 83L mutant among all UK isolates")+
  scale_fill_manual(values = mycolorsGyr, breaks = top_group_for_legend) +
  scale_y_continuous(limits = c(0,1))
dev.off()

table(UKMetaData$Collection.Year)