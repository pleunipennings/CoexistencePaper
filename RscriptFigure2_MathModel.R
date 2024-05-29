
#Make a plot for the relationship between t, e, c and f
library("RColorBrewer")
library("ggplot2")
library("tidyverse")
setwd("~/Documents/GitHub/CoexistencePaper")


tevalues <- seq(0.0,0.05, by = 0.005)
DF <- data.frame(te_value = rep(tevalues,9), c_value = 0, f_value = 0)
cvalues <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15, 0.25)
for (i in 0:8){
  DF$c_value[(i*11)+(1:11)]<-cvalues[i+1]
}
for (i in 1:nrow(DF)){
  DF$f_value[i]<-DF$te_value[i] / (DF$c_value[i])
}

DF$c_value_factor <- factor(DF$c_value)
DF<-DF %>% group_by(c_value_factor)

set.seed(30)
mycolorsMathFigures <- sample(colorRampPalette(brewer.pal(8, "Set3"))(30))

png(filename = "Figures/Figure2_EvolutionVsFractionResistant.png", width = 5, height = 4, units = "in", res = 300)

ggplot(data = DF, aes(x = te_value, y = f_value, group = c_value_factor, color = c_value_factor))+
  geom_line(linewidth = 1)+
  geom_abline(intercept= rep(0,9), slope = 1/cvalues, linetype = 1, 
              color = mycolorsMathFigures[1:9],
              linewidth = 1)+
  labs(color = "Cost of resistance")+
  scale_color_manual(values = mycolorsMathFigures) +
  theme_classic()+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, size = 1)) +
  labs(y= "Predicted fraction resistant", x = "Evolution probability t*e", color = "Cost of resistance")+
  scale_x_continuous(expand = c(0, 0), limits = c(0,0.05)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))

dev.off()

