#Colors for the sequence types

library("RColorBrewer")
library("ggplot2")
setwd("~/Documents/Selection-Selection-Balance-Model")


UKMetaDataForColors <- read.csv(file = "Enterobase/UKMetaData_withBlastResults.csv")

OrderST_100 <- names(sort(table(UKMetaDataForColors$ST_100), decreasing = TRUE))
top_group_for_legend <- OrderST_100[1:10]
top_group_for_legend_20 <- OrderST_100[1:20]
set.seed(seed = 30)
mycolorsST_100 <- sample(colorRampPalette(brewer.pal(8, "Set3"))(101))
names(mycolorsST_100) <- OrderST_100
