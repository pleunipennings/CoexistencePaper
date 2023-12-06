setwd("~/Documents/Selection-Selection-Balance-Model")
ListOfAbricateFiles <- list.files("Enterobase/AbricateResults/")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

###NOTE

#Read the first abricate file
AbrRes <- read.csv(paste0("Enterobase/AbricateResults/",ListOfAbricateFiles[1]))
ListResGenes <- data.frame(matrix(ncol = ncol(AbrRes), nrow = 0))
names(ListResGenes)<-names(AbrRes)
ListResGenes <- ListResGenes[,c("X.FILE", "GENE")]
ListNoResGenes<-c()

for (i in 1:length(ListOfAbricateFiles)){
#for (i in 1:1000){
  if (i%%100 ==0) print(paste(i, "out of ", length(ListOfAbricateFiles)))
  AbrRes <- read.csv(paste0("Enterobase/AbricateResults/",ListOfAbricateFiles[i]))
  if (nrow(AbrRes)>0){
    #print(i)
    for (j in 1:nrow(AbrRes)){
      ListResGenes <- add_row(ListResGenes)
      ListResGenes[nrow(ListResGenes),"X.FILE"] <- AbrRes[j,"X.FILE"]
      ListResGenes[nrow(ListResGenes),"GENE"] <- AbrRes[j,"GENE"]
    }
  }
  if (nrow(AbrRes)==0){
    ListNoResGenes<-c(ListNoResGenes, ListOfAbricateFiles[i])
  }
}

ListResGenes$assemblynames<- substr(ListResGenes$X.FILE,17,31)

ListCommonGenes <- tail(names(sort(table(ListResGenes$GENE))),20)

ListNoResGenes <- substr(ListNoResGenes,1,15)

#Next up, take the UKMetaData and add the Bla info
UKMetaDataAbr <-read.csv("Enterobase/UK_MetaData_ST_Barcode.txt", sep = "\t")
#Remove entries with no year 
UKMetaDataAbr<- UKMetaDataAbr[which(!is.na(UKMetaDataAbr$Collection.Year)),]
#Keep only the ones for which I have abricate data
UKMetaDataAbr <- UKMetaDataAbr[which(UKMetaDataAbr$Assembly.barcode.Assembly.stats. %in% substr(ListOfAbricateFiles,1,15)),]
#We got 38303 isolates with Abricate results
#Keep only the relevant columns 
UKMetaDataAbr <- UKMetaDataAbr[,names(UKMetaDataAbr)%in% c("ST_20", "ST_100","ST.Achtman.7.Gene.MLST.", "GyrA_AA83",
                                                                 "Assembly.barcode.Assembly.stats.",
                                                                 "Collection.Year" )]
#Now for each of the important genes, make a new column 
for (gene in c("blaTEM-1B_1", "blaCTX-M-15_1")){
  UKMetaDataAbr[gene]<-0
  #then for each of the rows in the UKMeta data frame, see if the gene is there in ListResGenes for that assembly
  for (i in 1:nrow(UKMetaDataAbr)){
    if (i%%100 ==0) print(paste("gene", gene, "i ", i, "out of ", nrow(UKMetaDataAbr)))
    assemblyname = UKMetaDataAbr$Assembly.barcode.Assembly.stats.[i]
    #rowsofinterest<-which(ListResGenes$assemblynames == assemblyname & ListResGenes$GENE == gene)
    if (length(which(ListResGenes$assemblynames == assemblyname))>0){
      if (gene %in% ListResGenes$GENE[ListResGenes$assemblynames == assemblyname])UKMetaDataAbr[i,gene]<-1
    }
  }
}

##Add ST_100 and ST_20 columns. 
#Make all ST from 21 onwards "Other" 
allSTs<-names(sort(table(UKMetaDataAbr$ST.Achtman.7.Gene.MLST.), decreasing = TRUE))

UKMetaDataAbr$ST_20<- as.character(UKMetaDataAbr$ST.Achtman.7.Gene.MLST.)
UKMetaDataAbr$ST_20[UKMetaDataAbr$ST_20 %in% allSTs[21:length(allSTs)]]<- "Other"
OrderSTsST_20 <- names(sort(table(UKMetaDataAbr$ST_20), decreasing = TRUE))
UKMetaDataAbr$ST_20<-factor(UKMetaDataAbr$ST_20, levels = OrderSTsST_20)

####FRAC RES 100 STs 
UKMetaDataAbr$ST_100<- as.character(UKMetaDataAbr$ST.Achtman.7.Gene.MLST.)
UKMetaDataAbr$ST_100[UKMetaDataAbr$ST_100 %in% allSTs[101:length(allSTs)]]<- "Other"
OrderSTsST_100 <- names(sort(table(UKMetaDataAbr$ST_100), decreasing = TRUE))
UKMetaDataAbr$ST_100<-factor(UKMetaDataAbr$ST_100, levels = OrderSTsST_100)

###HERE WRITE THE FILE
#Name should be 
filename = "Enterobase/MetaUKBlastAbricateResultsNov7_2023.csv"
write.csv(x = UKMetaDataAbr, file = filename, row.names = FALSE)

################THE END FOR NOW 

if (FALSE){
#Now, I'll repeat for Bla TEM and Bla CTX separately
UKMetaData69$BlaTEM <- NA
UKMetaData69$BlaCTX <- NA

for (i in 1: nrow(UKMetaData69)){
  assemblyname = UKMetaData69$Assembly.barcode.Assembly.stats.[i]
  if (assemblyname %in% substr(ListBlaGenes$X.FILE,17,31)){
    UKMetaData69$BlaTEM[i] = 0; UKMetaData69$BlaCTX[i] = 0;
    rowswithinfo <- which(substr(ListBlaGenes$X.FILE,17,31) == assemblyname)
    if (length(grep("TEM", ListBlaGenes$GENE[rowswithinfo]))>0) UKMetaData69$BlaTEM[i]<-1
    if (length(grep("CTX", ListBlaGenes$GENE[rowswithinfo]))>0) UKMetaData69$BlaCTX[i]<-1    
  }
  else {UKMetaData69$BlaTEM[i] = 0; UKMetaData69$BlaCTX[i] = 0;}
}

FracBlaTEM<-UKMetaData69 %>%
  group_by(Collection.Year, ST_simple) %>%
  summarise(Frac = sum(BlaTEM ==1, na.rm = TRUE), 
            Num = sum(BlaTEM ==1, na.rm = TRUE), 
            NumObs = sum(BlaTEM %in% 0:1), .groups = "keep") 
FracBlaTEM$Frac = FracBlaTEM$Frac/FracBlaTEM$NumObs

FracBlaCTX<-UKMetaData69 %>%
  group_by(Collection.Year, ST_simple) %>%
  summarise(Frac = sum(BlaCTX ==1, na.rm = TRUE), 
            Num = sum(BlaCTX ==1, na.rm = TRUE), 
            NumObs = sum(BlaCTX %in% 0:1), .groups = "keep") 
FracBlaCTX$Frac = FracBlaCTX$Frac/FracBlaCTX$NumObs

ggplot(data = FracBlaTEM[FracBlaCTX$ST_simple!="Other",], aes(Collection.Year, Frac))+
  geom_col(aes(Collection.Year, Frac, fill = as.factor(ST_simple)))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Fraction Bla gene among strains of the ST")


ggplot(data = FracBlaCTX[FracBlaCTX$ST_simple!="Other",], aes(Collection.Year, Frac))+
  geom_col(aes(Collection.Year, Frac, fill = as.factor(ST_simple)))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Fraction Bla gene among strains of the ST")


myTree <- ape::read.tree("phylo_tree69.nwk")
Tiplabels <- myTree$tip.label
myTree$tip.label <- rep("_",times = length(Tiplabels))

for (i in 1:nrow(ListBlaGenes)){
  assemblyname = substr(ListBlaGenes$X.FILE[i],17,31)
  which(Tiplabels == assemblyname)
  myTree$tip.label[which(Tiplabels == assemblyname)] <- paste0(myTree$tip.label[which(Tiplabels == assemblyname)],"_" , ListBlaGenes$GENE[i])
}

png("Tree69_BlaGenes.png", width = 10, height = 20, units = "in", res = 200)
plotTree(myTree, fsize = 0.5)
dev.off()

} 

