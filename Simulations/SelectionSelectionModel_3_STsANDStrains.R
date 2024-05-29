
#Wed Oct 4 2023
#Let's see if I can get a simple SIRS model going here. 
#I need to keep track of different sus and res strains. 

#This model will be individual based. 
#I'll start with 50,000 individuals. 
#0 is uninfected, 1 is infected with susceptible strain, 2 and further: resistant strains. 
#Let's seed the pop with a few susceptible infected strains, -1 is recovered. 

#Let's have the infected folks infect the uninfected folks. 
#This should happen at a rate of R0*NumUninfected/PopSize
#For each generation, the num ongoing infections is R0, and all infected people recover
#Recovered folks can become susceptible again

#OK, so I can create new infections, now let's see what happens to the total # infected

library("RColorBrewer")
library("ggplot2")
library("reshape")
setwd("~/Documents/Selection-Selection-Balance-Model")

#START HERE
TotalNumGenerations = 300
PopSize = 20000

#I want to see the proportions of the sequence types to use in the simulations
UKMetaData <- read.csv(file = "Enterobase/UKMetaData_withBlastResults.csv")
#I'll focus on the 100 most common sequence types
#table(UKMetaData$ST_100)/10
PopST <- rep("0",PopSize)
Pop <- rep(0,PopSize)
NumInfectedSeed <-0.2*PopSize #Starting with an equilibirum number of infections
Pop[1:NumInfectedSeed] <- 1 #1 means infected
PossibleSequenceTypes <- unique(UKMetaData$ST_100)
PossibleSequenceTypes<-PossibleSequenceTypes[PossibleSequenceTypes!="0"]
PossibleSequenceTypes<-PossibleSequenceTypes[!is.na(PossibleSequenceTypes)]

PopST [1:NumInfectedSeed] <- sample(PossibleSequenceTypes,NumInfectedSeed, replace = TRUE)
CostResistance = 0.05
R0sus <- 2 ; R0res <- (1-CostResistance)*R0sus
ExpectedNumGenImmune <- 5
RiskResistanceEvo = 0.2 *(1- R0res/R0sus) 
#RiskResistanceEvo = 0.006

NumInfectedOverTime <- c()
NumInfectedResOverTime <- c()
NumRecoveredOverTime <- c()
NumUnInfectedOverTime <- c()

TotalNumStrains = length(unique(PopST))+1
DF_StrainsSus <- data.frame(Strains = names(table(PopST)), Gen_0 = as.vector(table(PopST)))
DF_StrainsRes <- data.frame(Strains = names(table(PopST)), Gen_0 = c(rep(0,TotalNumStrains-1)))

for (t in 1:TotalNumGenerations){
  
  if (t%%10==0) print(paste0("generation ", t))
  
  NumInfectedOverTime <- c(NumInfectedOverTime, length(which(Pop >0)))
  NumInfectedResOverTime <-c(NumInfectedResOverTime, length(which(Pop >1)))
  NumRecoveredOverTime <- c(NumRecoveredOverTime, length(which(Pop %in% -1)))
  NumUnInfectedOverTime <- c(NumUnInfectedOverTime, length(which(Pop %in% 0)))
  
  #Loop over the recovered people to make them uninfected and susceptible again
  for (j in which(Pop %in% -1)){
    if (runif(n=1) < 1/ExpectedNumGenImmune) Pop[j] <- 0 #recovered becomes uninfected
  }
  
  #Loop over the infected people to create new infections
  for (i in which(Pop >0)){
    ExpectedNumNewInfections <- R0sus*length(which(Pop==0))/PopSize #if infection is with susceptible strain
    if (Pop[i]>1) ExpectedNumNewInfections <- R0res*length(which(Pop==0))/PopSize #lower R0 for the resistant strains
    #print(ExpectedNumNewInfections)
    NumNewInfections = rpois(n = 1, lambda = ExpectedNumNewInfections) #decide on num of new infections
    NewlyInfecteds <- sample(x = which(Pop==0), size = NumNewInfections) #decide on which inds become infected
    Pop[NewlyInfecteds] <- Pop[i] #infect the newly infected people with the strain they had
    PopST[NewlyInfecteds] <- PopST[i] #infect the newly infected people with the strain they had
    Pop[i] <- -1 #recover the previously infected person
  }
  
  #Loop over the infected people with susceptible strain (1) to allow evolution of resistance
  if (t > 25){
    for (h in which(Pop %in% 1)){ #only those infected with susceptible pathogen strain
      #NumberNextResStrain = max(Pop) + 1 #don't need this here because we keep track of the ST
      if (runif(n=1) < RiskResistanceEvo) Pop[h] <- 2 #sus becomes res 
      #print(NumberNextResStrain)
    }
  }
  
  #Fill Strains data frame
  DF_StrainsSus[paste0("Gen_", t)] <- 0
  DF_StrainsRes[paste0("Gen_", t)] <- 0
  table(PopST[Pop==1]) ->tableSusStrains
  table(PopST[Pop==2]) ->tableResStrains
  
  DF_StrainsSus[DF_StrainsSus$Strains%in%names(tableSusStrains), ncol(DF_StrainsSus)] <- as.vector(tableSusStrains)
  DF_StrainsRes[DF_StrainsRes$Strains%in%names(tableResStrains), ncol(DF_StrainsRes)] <- as.vector(tableResStrains)
}

par(mar = c(1, 1, 1, 1)) # Set the margin on all sides to 2
par(mfrow = c(1, 3))

plot(NumInfectedOverTime, type = "l", ylim = c(0,PopSize))
points(NumInfectedResOverTime, type = "l", col = 2)
points(NumRecoveredOverTime, type = "l", col = 3)
points(NumUnInfectedOverTime, type = "l", col = 4)

plot(NumInfectedOverTime, type = "l", ylim = c(0, max(NumInfectedOverTime)))
points(NumInfectedResOverTime, type = "l", col = 2)
title("Black = total infected, red = resistant strains")

plot(NumInfectedResOverTime/NumInfectedOverTime, type = "l")
abline(h = RiskResistanceEvo / CostResistance, col  = 2)
title("fraction resistant (predicted and observed)")

genstoshow <- c(seq(1, length(NumInfectedResOverTime), by = 10),TotalNumGenerations)

par(mfrow = c(1, 1))

DF_run <- data.frame(Generation = 1:TotalNumGenerations, NumInfectedOverTime, NumInfectedResOverTime, NumRecoveredOverTime, NumUnInfectedOverTime)
DF_run_sparse <- DF_run[genstoshow,]

ggplot(data = DF_run_sparse, aes(x = Generation, y = NumInfectedOverTime)) +
  geom_area(fill = "darkolivegreen3", alpha = 1) + 
  geom_area(aes(x = Generation, y = NumInfectedResOverTime), fill="forestgreen", alpha = 1) 

TransposedStrains <- t(DF_StrainsRes[2:nrow(DF_StrainsRes),2:ncol(DF_StrainsRes)]) #Only keeping the resistant strains and transposing
colnames(TransposedStrains)<- DF_StrainsRes$Strains[2:nrow(DF_StrainsRes)]
TransposedStrains<-as.data.frame(TransposedStrains)
TransposedStrains$Gen <- 1:nrow(TransposedStrains)
TransposedStrains$Sus_OverTime <- c(900,DF_run$NumInfectedOverTime-DF_run$NumInfectedResOverTime)

data_long <- melt(TransposedStrains, id = "Gen") 
data_long <- data_long[data_long$Gen%in%genstoshow,]

data_long_sparse <- data_long[data_long$Gen %in% genstoshow,]

data_long_sparse$FracOfAll<-NA
for (g in unique(data_long_sparse$Gen)){
  Total = sum(data_long_sparse$value[data_long_sparse$Gen==g], na.rm = TRUE)
  data_long_sparse$FracOfAll[data_long_sparse$Gen==g]<- data_long_sparse$value[data_long_sparse$Gen==g]/Total
}

SimpleDF <- data.frame(t = 1:length(NumInfectedResOverTime), NumInfectedResOverTime, NumInfectedOverTime)

#png(filename = "FractionResistant.png", width = 3, height = 3, units = "in", res = 300)
ggplot(SimpleDF,             
       aes(x = t, y = NumInfectedResOverTime/NumInfectedOverTime), show.legend = FALSE) +  
  geom_line() +
  theme_minimal()+
  scale_x_continuous(limits = c(0,TotalNumGenerations))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(y= "Fraction Resistant", x = "Time")+
  geom_hline(yintercept = RiskResistanceEvo / ((1- R0res/R0sus))) 
  #geom_hline(yintercept = RiskResistanceEvo / (1- R0res/R0sus), color = "red")
#dev.off()

write.csv(file = "ResultsSimulations/DataLongDec4_2023.csv", x = data_long_sparse)
