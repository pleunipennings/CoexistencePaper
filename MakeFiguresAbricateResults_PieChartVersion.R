###HERE READ THE FILE NEW R SCRIPT 

setwd("~/Documents/Selection-Selection-Balance-Model")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

#Get the colors for the sequence types
source("Rscripts_Simulations_Coexistence/ColorsSeqTypes.R")

#Name should be 
filename = "Enterobase/MetaUKBlastAbricateResultsNov7_2023.csv"
MetaUKBlastAbr2 <- read.csv(file = filename)
ListCommonGenes<- c("blaTEM.1B_1", "blaCTX.M.15_1")

#Now summarize by year and ST for each gene of interest
FracResGenes<-MetaUKBlastAbr2 %>%
  group_by(Collection.Year, ST_100) %>%
  summarise(across(all_of(ListCommonGenes), ~ mean(.x, na.rm = TRUE)), .groups = "keep")

FracResGenes_longer <- FracResGenes %>%
  pivot_longer(cols = names(FracResGenes)[3:ncol(FracResGenes)], names_to = "Genes", values_to = "Frac")

###Now get FRACTION of the whole. 
years = unique(FracResGenes_longer$Collection.Year)

FracResGenes_longer$FracOfAll <- NA
FracResGenes_longer$FracSusOfAll <- NA

for (y in years){
  print(y)
  for (gene in unique(FracResGenes_longer$Genes)){
    Total = length(MetaUKBlastAbr2[MetaUKBlastAbr2$Collection.Year == y,gene])
    for (ST in unique(FracResGenes_longer$ST_100)){
      TotalHasGene = length(which(MetaUKBlastAbr2[MetaUKBlastAbr2$Collection.Year == y & MetaUKBlastAbr2$ST_100==ST,gene]==1))
      FracResGenes_longer$FracOfAll[FracResGenes_longer$Collection.Year==y & FracResGenes_longer$Genes==gene & FracResGenes_longer$ST_100==ST] <- TotalHasGene/Total
      TotalNotHasGene = length(which(MetaUKBlastAbr2[MetaUKBlastAbr2$Collection.Year == y & MetaUKBlastAbr2$ST_100==ST,gene]==0))
      FracResGenes_longer$FracSusOfAll[FracResGenes_longer$Collection.Year==y & FracResGenes_longer$Genes==gene & FracResGenes_longer$ST_100==ST] <-TotalNotHasGene/Total
      if (length(which(FracResGenes_longer$Collection.Year==y & FracResGenes_longer$Genes==gene & FracResGenes_longer$ST_100==ST))==0){
        #this occurs when there are no observations for a ST for a gene in a year
        FracResGenes_longer=rbind(FracResGenes_longer, data.frame(Collection.Year = y, ST_100 = ST, Genes = gene, Frac = NA, FracOfAll = 0, FracSusOfAll = 0)) 
      }
    }
  }
}

#ListGenesToLoopOver<-unique(FracResGenes_longer$Genes)

years<- 2008:2022
df_Totals<-data.frame(Years = years, Num = rep(1,length(years)))

OrderSTs <- names(sort(table(MetaUKBlastAbr2$ST_100), decreasing = TRUE)) ## niet consistent met kleuren
FracResGenes_longer$ST_100<-factor(FracResGenes_longer$ST_100, levels = OrderSTs)

png(file = "Enterobase/Figures_EnteroBase/AbricateResults_Black.png", width = 9, height = 3, units = "in", res = 300 )
ggplot(data = FracResGenes_longer[FracResGenes_longer$Collection.Year %in% years & FracResGenes_longer$Genes == "blaTEM.1B_1",], aes(Collection.Year, FracOfAll))+
  geom_col(data = df_Totals, aes(Years, Num), fill = "646464")+
  #geom_col(aes(Collection.Year, FracOfAll, fill = as.factor(ST_100)))+
  geom_col(aes(Collection.Year, FracOfAll), fill = "black")+
  theme_bw()+
  theme(legend.position = "right")+
  ylab("Fraction with resistance")+
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend) +
  scale_y_continuous(limits = c(0,1))+
#  facet_wrap("Genes")+
  labs(fill = "Common ST") 
dev.off()

FracResGenes_longer$label_vector = as.character(FracResGenes_longer$ST_100)
FracResGenes_longer$label_vector[FracResGenes_longer$FracOfAll<0.007]<-""
FracResGenes_longer$color <- mycolorsST_100[FracResGenes_longer$ST_100]
FracResGenes_longer$FracOfAll[is.na(FracResGenes_longer$FracOfAll)]<-0

png(file = "Enterobase/Figures_EnteroBase/AbricateResultsBlaTEM_PieCharts.png", width = 8, height = 3.5, units = "in", res = 300 )
par(mar = c(1, 1, 1, 1)) # Set the margin on all sides to 2
par(mfrow = c(1, 4))
for (yeartoPie in c(2009, 2012, 2018, 2021)){ 
genetoPie = "blaTEM.1B_1"
#genetoPie = "blaCTX.M.15_1"
pie(FracResGenes_longer$FracOfAll[FracResGenes_longer$Collection.Year==yeartoPie&
                                    FracResGenes_longer$Genes == genetoPie], 
  col = FracResGenes_longer$color[FracResGenes_longer$Collection.Year==yeartoPie&
                                        FracResGenes_longer$Genes == genetoPie],
  labels = FracResGenes_longer$label_vector[FracResGenes_longer$Collection.Year==yeartoPie&
                                       FracResGenes_longer$Genes == genetoPie], 
  main = paste(yeartoPie)
  )
}
dev.off()

#install.packages("remotes")
#remotes::install_github("davidsjoberg/ggsankey")

library(ggsankey)

years_3 <- c(2009, 2012, 2015, 2018, 2021)
FracResGenes_Ribbon <- FracResGenes_longer[FracResGenes_longer$Collection.Year %in% years_3 & FracResGenes_longer$Genes == "blaTEM.1B_1" & FracResGenes_longer$ST_100!="Other",]
FracResGenes_Ribbon<-FracResGenes_Ribbon %>% group_by(ST_100)
FracResGenes_Ribbon<-FracResGenes_Ribbon[order(FracResGenes_Ribbon$FracOfAll,decreasing = TRUE),]
top_group_for_legend_Ribbon <- unique(FracResGenes_Ribbon$ST_100[FracResGenes_Ribbon$FracOfAll>0.005])

#png(file = "Enterobase/Figures_EnteroBase/AbricateResultsBlaTEM_sankey.png", width = 8, height = 3.5, units = "in", res = 300 )
ggplot(data = FracResGenes_Ribbon, 
       aes(x = Collection.Year, value = FracOfAll, fill = ST_100, node = ST_100))+
  #geom_col(data = df_Totals, aes(Years, Num), fill = "646464")+
  geom_sankey_bump(space = 0, type = "alluvial", color = "darkgrey", 
                   smooth = 10, flow_start_ymax = 0, flow_start_ymin = 0, 
                   flow_end_ymax = 0, flow_end_ymin = 0, 
                   position = "identity",
                   na.rm = FALSE) +
    #scale_fill_viridis_d(option = "A", alpha = .8) +
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend_Ribbon) +
  #theme_sankey_bump(base_size = 16) +
  labs(x = NULL,
       y = "Resistant isolates",
       fill = "Sequence Types",
       color = NULL) +
  theme_bw()+
  theme(legend.position = "bottom") +
  labs(title = "Most common sequence types of resistant isolates")+
  scale_x_continuous(breaks = years_3, labels = years_3)+
  scale_y_continuous(limits=c(0,0.25))
#dev.off()

#png(file = "Enterobase/Figures_EnteroBase/AbricateResultsNO_BlaTEM_sankey.png", width = 8, height = 3.5, units = "in", res = 300 )
ggplot(data = FracResGenes_Ribbon, 
       aes(x = Collection.Year, value = FracSusOfAll, fill = ST_100, node = ST_100))+
  #geom_col(data = df_Totals, aes(Years, Num), fill = "646464")+
  geom_sankey_bump(space = 0, type = "alluvial", color = "grey", 
                   smooth = 10, flow_start_ymax = 0, flow_start_ymin = 0, 
                   flow_end_ymax = 0, flow_end_ymin = 0, 
                   position = "identity",
                   na.rm = FALSE) +
  #scale_fill_viridis_d(option = "A", alpha = .8) +
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend_Ribbon) +
  #theme_sankey_bump(base_size = 16) +
  labs(x = NULL,
       y = "Resistant isolates",
       fill = "Sequence Types",
       color = NULL) +
  theme_bw()+
  theme(legend.position = "bottom") +
  labs(title = "Most common sequence types of suscaptible isolates")+
  scale_x_continuous(breaks = years_3, labels = years_3)+
  scale_y_continuous(limits=c(0,1))
#dev.off()

#png(file = "Enterobase/Figures_EnteroBase/AbricateResultsBlaTEM.png", width = 8, height = 4, units = "in", res = 300 )
#Same colors as previous plot
ggplot(data = FracResGenes_longer[FracResGenes_longer$Collection.Year %in% years & FracResGenes_longer$Genes == "blaTEM.1B_1",], aes(Collection.Year, FracOfAll))+
  geom_col(data = df_Totals, aes(Years, Num), fill = "646464")+
  geom_col(aes(Collection.Year, FracOfAll, fill = as.factor(ST_100)))+
  theme_classic()+
  ylab("Fraction isolates with BlaTEM-1B gene")+
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend) +
  labs(fill = "Common ST") 
#dev.off()

#png(file = "Enterobase/Figures_EnteroBase/AbricateResultsBlaCTX.png", width = 8, height = 4, units = "in", res = 300 )
#Same colors as previous plot
ggplot(data = FracResGenes_longer[FracResGenes_longer$Collection.Year %in% years & FracResGenes_longer$Genes == "blaCTX.M.15_1",], aes(Collection.Year, FracOfAll))+
  geom_col(data = df_Totals, aes(Years, Num), fill = "646464")+
  geom_col(aes(Collection.Year, FracOfAll, fill = as.factor(ST_100)))+
  theme_classic()+
  ylab("Fraction isolates with BlaCTX-M15 gene")+
  scale_fill_manual(values = mycolorsST_100, breaks = top_group_for_legend) +
  labs(fill = "Common ST") 
#dev.off()




