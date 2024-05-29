library(adegenet); library(ape); 
library(cowplot); library(seqinr)
library(gridExtra); library(ggpubr); library(stringr); library("geiger")
#library(phangorn); 
library("phytools"); library("lmtest"); library("treedater")
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(gridGraphics)
library(stringr)

####################################################
####
##Read the tree file and the meta data
####
####################################################

setwd("~/Documents/GitHub/CoexistencePaper/")
#Read the meta data including sequence / sample names and mutation identity (does it have the 83L in Gyrase A?)
NorwayMetaData = read.csv(file = "NorwayData/EMS184826-supplement-Sample_metadata.csv")
## Create Label column 
NorwayMetaData$Label <- gsub("#", "_", NorwayMetaData$lane) 
## Keep only some of the columns 
ColumnsToKeep <- c("Label", "ST" , "year", "Ciprofloxacin_SIR" , "Cefotaxim_SIR" , "Gentamicin_SIR", "Ampicillin_SIR", "Piperacillin_tazobactam_SIR")
NorwayMetaData <- NorwayMetaData[, which(names(NorwayMetaData)%in%ColumnsToKeep)]

#Read tree file (nwk)
myTree <- ape::read.tree(file = "NorwayData/Gladstone2021_tree.nwk")

length(which(myTree$tip.label%in%NorwayMetaData$Label)) ##Make sure all tree labels are in the metadata 
length(which(NorwayMetaData$Label%in%myTree$tip.label)) ##and VV
NorwayMetaData <- NorwayMetaData[match(myTree$tip.label, NorwayMetaData$Label),] ##Order the dataframe so that it is in the order of the tree file. 

####################################################
####
##For each of the 5 resistances ("Ciprofloxacin_SIR" , "Cefotaxim_SIR" , "Gentamicin_SIR", "Ampicillin_SIR", Piperacillin_tazobactam_SIR) do the following steps: 
#1. Make resistance array and y_list 
#2. make.simmap (this simulates the evolution of phenotypes on the tree based on the tips)
#3. plot tree
####
####################################################

####################################################
####
#1. Make resistance array and y_list 
Resistances = c("Ciprofloxacin_SIR" , "Cefotaxim_SIR" , "Gentamicin_SIR", "Ampicillin_SIR", "Piperacillin_tazobactam_SIR")
###Start with Cipro
DrugResistance = Resistances[5]

#Will create an array with resistance info 
ResistanceArray = NorwayMetaData[[DrugResistance]]
ResistanceArray[which(ResistanceArray%in% c("I", "S"))] <- "IS" ##Intermediate and Susceptible become one class
Yearvector = NorwayMetaData$year

#Remove the missing data from the tree and the Resistance and Year vectors. 
if (length(which(ResistanceArray ==""))>0){
  toremove = which(ResistanceArray =="")
  myTreeTrimmed = ape::drop.tip(myTree, toremove)    
  ResistanceArray <- ResistanceArray[-toremove]
  Yearvector <- Yearvector[-toremove]
} else {myTreeTrimmed = myTree}

#fraction resistant (L) out of those with L or S: 0.148 (nearly 15% resistant)
round(length(which(ResistanceArray=="R")) / length(ResistanceArray),3)
Yearvector<-setNames(Yearvector,myTreeTrimmed$tip.label)
y_list <- setNames(ResistanceArray,myTreeTrimmed$tip.label) ## y_list is used as such by the phytools tutorials

####################################################
####
## 2. simulate single stochastic character map 
#mySimmapTree<-make.simmap(myTreeTrimmed,y_list,model="ER") 
#f pi="fitzjohn", then the Fitzjohn et al. (2009) root prior is used.
#This takes a few minutes to run

simmap.tree<-make.simmap(myTreeTrimmed,y_list,model="ARD",
                          pi="fitzjohn",nsim=1)
####################################################
####
#3. Plot tree 

pdf(paste0("Figures/AdditionalNorwayFigures/Tree_Norway",DrugResistance,".pdf"), width = 20, height = 20)
par(mfrow=c(1,1))
cols<-setNames(c("darkgrey", "red"),levels(as.factor(y_list)))
plot(simmap.tree, colors = cols, lwd=.5,ftype="off")
legend("bottomleft",c("Non resistant", "Resistant"),pch=21,pt.bg=cols,
       pt.cex=2,bty="n")
dev.off()

####################################################
####
##For each of the 5 resistances ("Ciprofloxacin_SIR" , "Cefotaxim_SIR" , "Gentamicin_SIR", "Ampicillin_SIR", Piperacillin_tazobactam_SIR) do the following steps: 
#4. Count cluster sizes 
#5. Count lifespan of clusters 
#6. Plot some of that  
####
####################################################

#get the resistant leaves of the tree
resleaves = which(getStates(simmap.tree,"tips")=="R")
clustersizes = c(); listToIgnore = c(); resnodesclusters = c(); lastresistantparents = c()

#For each res leave get the last resistant ancestor. 
#Two res leaves with the same last resistant ancestor are considered to be in the same cluster
for (resleave in  resleaves){
  parent = getParent(simmap.tree, resleave) 
  #find the last resistant parent, if singleton, then last resistant parent is itself
  #if parent not resistant, then singleton clade 
  if (getStates(simmap.tree,"nodes")[as.character(parent)] %in% c("IS")) { 
    lastresistantparents <-c(lastresistantparents, resleave)
  }
  #if parent resistant, count clade size 
  if (getStates(simmap.tree,"nodes")[as.character(parent)] == "R") {
    #if not a singleton, get the last resistant parent (going back in time)
    while (getStates(simmap.tree,"nodes")[as.character(parent)] == "R"){
      previousparent = parent
      if (is.null(getParent(simmap.tree,previousparent))) break() #If the parent is the root, we'll take the root as the last resistant parent
      parent = getParent(simmap.tree, parent) #I got to remember what the previous parent was though! 
      #print(paste(previousparent, parent))
    }
    lastresistantparent =   previousparent
    lastresistantparents <-c(lastresistantparents, lastresistantparent)
  }
}

#Collect cluster sizes and cluster names (last resistant parents)
clustersizes = as.data.frame(table(lastresistantparents))$Freq
resnodesclusters = as.character(as.data.frame(table(lastresistantparents))$lastresistantparents)

print("clustersizes:")
print(clustersizes)

#Organize clusters in a DF
ClusterSizeDF<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ST", "clustersize", "lastresistantparent"))

for (c in 1: length(clustersizes)){
  ClusterSizeDF[nrow(ClusterSizeDF)+1,]<-list("", clustersizes[c], resnodesclusters[c])
}

####Add cluster size info to the MetaData dataframe
NorwayMetaData[[paste0("Clusters",DrugResistance)]]<-0
for (i in 1: length(resleaves)){
  Label = names(resleaves)[i]
  Cluster = lastresistantparents[i]
  NorwayMetaData[[paste0("Clusters",DrugResistance)]][which(NorwayMetaData$Label == Label)] <- Cluster
}

####For each cluster, get the most common ST 
for (i in 1:length(ClusterSizeDF$lastresistantparent)){
  c = ClusterSizeDF$lastresistantparent[i]
  rowsofthiscluster = which(NorwayMetaData[[paste0("Clusters",DrugResistance)]]==c)
  #print(NorwayMetaData$ST[rowsofthiscluster])
  ClusterSizeDF$ST[i]<-names(sort(table(NorwayMetaData$ST[rowsofthiscluster]),decreasing=TRUE)[1])
}

#Now summarise the data per year and by cluster 
ClusterList <- unique(NorwayMetaData[[paste0("Clusters",DrugResistance)]])
YearList <- sort(unique(NorwayMetaData$year))
#Create a new overview dataframe 
FractionsDF<-setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("ST", "Year", "Cluster", "Fraction", "Num", "TotalYearCluster"))

#For each cluster and each year determine the fraction of the all samples that is part of that cluster
#This will be used for figure 3D
for (c in ClusterList){
  for (y in YearList){
    TotalYear = length(which(NorwayMetaData$year==y))
    TotalYearCluster = length(which(NorwayMetaData$year==y & NorwayMetaData[[paste0("Clusters",DrugResistance)]]==c))
    FractionsDF[nrow(FractionsDF)+1,]<-list(0, y, c, TotalYearCluster/TotalYear, TotalYear, TotalYearCluster)
  }
}
FractionsDF$Cluster<-as.factor(FractionsDF$Cluster)

#for each cluster find most common ST and add that to FractionsDF
STList <- c()
for (c in ClusterList){
  if (c ==0){STList <-c(STList, NA)}
  if (c !=0){
    MostCommonSTForCluster <- as.numeric(names(sort(table(NorwayMetaData$ST [which(NorwayMetaData[[paste0("Clusters",DrugResistance)]] == c)]),decreasing=TRUE)[1]))
    FractionsDF$ST[FractionsDF$Cluster==c] <- MostCommonSTForCluster
    STList <- c(STList, MostCommonSTForCluster)
  }
}
FractionsDF$ST<- as.factor(FractionsDF$ST)

#####################################################
######
##Make plots for frequency over time for each cluster
######
#####################################################

png(paste0("Figures/AdditionalNorwayFigures/Norway_",DrugResistance,"_Frequencies.png"), width = 9, height = 20, units = "in", res = 200)
ggplot(FractionsDF[FractionsDF$ST!=0,], aes(Year, Fraction, col = ST))+
  geom_point(size = .7, show.legend = FALSE)+
  geom_line(lwd = .7, show.legend = FALSE)+
  theme_bw()+
  scale_x_continuous(limits = c(2002,2017))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  facet_wrap(~Cluster)+
  labs (y = "frequency individual alleles")
  #ggtitle(paste0("Occurrance of individal L alleles over time in ST", SequenceType))
dev.off()

#For each cluster, how long do we see it? 
ClusterSizeDF$CLusterSurvivalTime <- 0
for (i in 1:nrow(ClusterSizeDF)){
  Cluster = ClusterSizeDF$lastresistantparent[i]
  F <- FractionsDF[FractionsDF$Cluster==Cluster,]
  ClusterSizeDF$CLusterSurvivalTime[i] <- max(F$Year[which(F$Fraction>0)])-min(F$Year[which(F$Fraction>0)])+1
}

write.csv(x = FractionsDF, file = paste0("NorwayData/FractionsDF_",DrugResistance,".csv"))
write.csv(x = ClusterSizeDF, file = paste0("NorwayData/CLusterSizeDF_",DrugResistance,".csv"))

#####################################################
######
##Make simple plots for survival times and cluster size distribution
######
#####################################################

PossibleSurvivalTimes = 1:(max(ClusterSizeDF$CLusterSurvivalTime))
TableSurvivalTimes = c()
for (t in PossibleSurvivalTimes){
  TableSurvivalTimes = c(TableSurvivalTimes,length(which(ClusterSizeDF$CLusterSurvivalTime==t)))
}

png(paste0("Figures/AdditionalNorwayFigures/Norway_",DrugResistance,"_SurvivalTimes.png"), width = 6, height = 6, units = "in", res = 200)
plot(PossibleSurvivalTimes, TableSurvivalTimes, type = "b", pch = 15, 
     main = paste0(DrugResistance, " SurvivalTimes"))
abline ("h"=0)
dev.off()

PossibleClusterSizes = 1:(max(ClusterSizeDF$clustersize))
TableClusterSizes = c()
for (s in PossibleClusterSizes){
  TableClusterSizes = c(TableClusterSizes,length(which(ClusterSizeDF$clustersize==s)))
}
names(TableClusterSizes)<-1:length(TableClusterSizes)

png(paste0("Figures/AdditionalNorwayFigures/Norway_",DrugResistance,"_ClusterSizes.png"), width = 6, height = 6, units = "in", res = 200)
barplot(TableClusterSizes,
     main = paste0(DrugResistance, " ClusterSizes"), 
     xlim = c(1,150))
dev.off()

