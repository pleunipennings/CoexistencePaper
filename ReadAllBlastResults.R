
setwd("~/Documents/Selection-Selection-Balance-Model/Enterobase")
#library(tidyverse)
library(dplyr)
library(ggplot2)
library(seqinr)

BlastResultFiles <- list.files("AllBlastResults/")
ListAssembliesBlastResults <- substr(BlastResultFiles,1, 15)

UKMetaData <-read.csv("UK_MetaData_ST_Barcode.txt", sep = "\t")
OrderSTs <- names(sort(table(UKMetaData$ST), decreasing = TRUE))
UKMetaData$ST.Achtman.7.Gene.MLST.<- factor(UKMetaData$ST.Achtman.7.Gene.MLST., levels= OrderSTs)

#Only keep the metadata for the ones for which I have a blast result
ListMetaDataWithAssemblies <- which(UKMetaData$Assembly.barcode.Assembly.stats. %in% ListAssembliesBlastResults)
UKMetaData <- UKMetaData[ListMetaDataWithAssemblies,]

#Create new columns for Gyrase A mutations
UKMetaData$GyrA_AA83 <- ""
UKMetaData$GyrA_AA87 <- ""

for (i in 1:nrow(UKMetaData)){
  #find the assembly number 
  if (i %% 100 == 0 ) print(i)
  Assem_Meta <- UKMetaData$Assembly.barcode.Assembly.stats.[i]
  NumBlastFile <- which(substr(BlastResultFiles,1,nchar(Assem_Meta))==Assem_Meta)
  filename = paste0("AllBlastResults/", BlastResultFiles[NumBlastFile],sep = "")
  blastresult<-readLines(filename)[1]
  # determine the AA at 83 
  UKMetaData$GyrA_AA83[i]<- translate(s2c(blastresult))[83]
  # determine the AA at 87
  UKMetaData$GyrA_AA87[i]<- translate(s2c(blastresult))[87]
}

#UKMetaData <- UKMetaData[UKMetaData$Collection.Year>2007,]
allSTs<-names(sort(table(UKMetaData$ST.Achtman.7.Gene.MLST.), decreasing = TRUE))
#allSTs[21:length(allSTs)]

#FracRes 20 ST 
#Make all ST from 21 onwards "Other" 
UKMetaData$ST_20<- as.character(UKMetaData$ST.Achtman.7.Gene.MLST.)
UKMetaData$ST_20[UKMetaData$ST_20 %in% allSTs[21:length(allSTs)]]<- "Other"
OrderSTsST_20 <- names(sort(table(UKMetaData$ST_20), decreasing = TRUE))
UKMetaData$ST_20<-factor(UKMetaData$ST_20, levels = OrderSTsST_20)

####FRAC RES 100 STs 
UKMetaData$ST_100<- as.character(UKMetaData$ST.Achtman.7.Gene.MLST.)
UKMetaData$ST_100[UKMetaData$ST_100 %in% allSTs[101:length(allSTs)]]<- "Other"
OrderSTsST_100 <- names(sort(table(UKMetaData$ST_100), decreasing = TRUE))
UKMetaData$ST_100<-factor(UKMetaData$ST_100, levels = OrderSTsST_100)

write.csv(x = UKMetaData, file = "UKMetaData_withBlastResults.csv", row.names = FALSE)

