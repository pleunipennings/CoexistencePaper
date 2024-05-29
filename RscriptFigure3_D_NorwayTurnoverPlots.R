
setwd("~/Documents/GitHub/CoexistencePaper/")
library(ggplot2)

CiproClusters <- read.csv("NorwayData/FractionsDF_Ciprofloxacin_SIR.csv")
CiproClusters <- CiproClusters[which(CiproClusters$Cluster!=0),]

ClusterNames <- as.numeric(as.character(unique(CiproClusters$Cluster)))

CiproClusters$Cluster <- factor(CiproClusters$Cluster, levels = ClusterNames)

nb.cols = length(unique(CiproClusters$Cluster))
set.seed(2)
mycolors <- sample(colorRampPalette(brewer.pal(8, "Set3"))(nb.cols))

png(paste0("Figures/Figure_3D_Norway_TurnoverClusters.png"), width = 5.5, height = 5.5, units = "in", res = 200)

ggplot(data = CiproClusters, aes(x = Year, y = Fraction, fill = Cluster))+
  geom_area(show.legend = FALSE, color = "black")+
  scale_fill_manual(values = mycolors) +
  theme_bw()+
  ggtitle("Turnover of ciprofloxacin resistant strains")
dev.off()

