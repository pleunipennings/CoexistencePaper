
#Let's read the data and make the plot for the simulations
library("RColorBrewer")
library("ggplot2")
setwd("~/Documents/Selection-Selection-Balance-Model")
#Get the colors for the sequence types
source("Rscripts_Simulations_Coexistence/ColorsSeqTypes.R")
#use mycolorsST_100 for the colors 

#Read the results of the simulations
data_long_sparse<-read.csv("ResultsSimulations/DataLongDec4_2023.csv")
TotalNumGenerations = max(data_long_sparse$Gen)

data_long_sparse$OneMinusFracOfAll <- 1- data_long_sparse$FracOfAll

df_TotalsGen<-data.frame(Gen = unique(data_long_sparse$Gen), Num = rep(1,length(unique(data_long_sparse$Gen))))

mycolorsBlack <- rep(x = "black", times = length(mycolorsST_100))
variablenames = c("Simulations")

png(filename = "FiguresSimulations_ECDCData/SimulationEvolutionSelectionBalance_Black.png", width = 9, height = 3, units = "in", res = 300)
ggplot(data = data_long_sparse[data_long_sparse$variable =="Sus_OverTime",], aes(x = Gen, y = FracOfAll, fill = "variable"), show.legend = FALSE) +  
  geom_col(data = df_TotalsGen, aes(Gen, Num), fill = "646464")+
  #geom_col(position = position_stack(reverse = TRUE)) +
  geom_col(aes(Gen, OneMinusFracOfAll), fill = "black")+
  scale_x_continuous(limits = c(45,178))+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()+
  labs(y= "Fraction with resistance", x = "Time")+
  #scale_fill_manual(values = "black", breaks = top_group_for_legend) +
  scale_fill_manual(values = mycolorsBlack, breaks = top_group_for_legend) +
  labs(fill = "Common ST") 
dev.off()

data_long_sparse$label_vector = as.character(data_long_sparse$variable)
data_long_sparse$label_vector[data_long_sparse$FracOfAll<0.007]<-""
data_long_sparse$color <- mycolorsST_100[data_long_sparse$variable]
data_long_sparse$FracOfAll[is.na(data_long_sparse$FracOfAll)]<-0

png(filename = "FiguresSimulations_ECDCData/SimulationEvolutionSelectionBalance_Pies.png", width = 8, height = 3, units = "in", res = 300)
par(mar = c(1, 1, 1, 1)) # Set the margin on all sides to 2
par(mfrow = c(1, 4))
for (gentoPie in +c(71,101, 131, 161)){ 
  pie(data_long_sparse$FracOfAll[data_long_sparse$Gen==gentoPie & data_long_sparse$variable!="Sus_OverTime"], 
      col = data_long_sparse$color[data_long_sparse$Gen==gentoPie& data_long_sparse$variable!="Sus_OverTime"],
      labels = data_long_sparse$label_vector[data_long_sparse$Gen==gentoPie& data_long_sparse$variable!="Sus_OverTime"], 
      main = paste(gentoPie))
}
dev.off()

ggplot(data = data_long_sparse, aes(x = Gen, y = FracOfAll, fill = variable), show.legend = FALSE) +  
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(limits = c(0,300))+
  scale_y_continuous(limits = c(0,0.5))+
  theme_classic()+
  labs(y= "Fraction isolates with resistance", x = "Time")+
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend) +
  labs(fill = "Common ST") 


#Take 20 strains at random
TwentySStrains<-sample(unique(data_long_sparse$variable),20)
TwoStrains <- c("131", "11")

#png(file = "SimResults_TwoSTs_Nov6_2023.png", width = 4, height = 2, units = "in", res = 300 )

ggplot(data = data_long_sparse[data_long_sparse$variable %in% TwoStrains,], aes(Gen, FracOfAll))+
  geom_area(aes(Gen, FracOfAll, fill = as.factor(variable)))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y= "Fraction resistant", x = "Time")+
  scale_fill_manual(values = mycolorsST_100[which(names(mycolorsST_100) %in% TwoStrains)]) +
  scale_x_continuous(limits = c(100,300))+
  facet_wrap("variable", scales = "free")

#dev.off()
