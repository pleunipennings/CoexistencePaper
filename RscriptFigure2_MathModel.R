
#Make a plot for the relationship between t, e, c and f
library("RColorBrewer")
library("ggplot2")
library("tidyverse")
setwd("~/Documents/GitHub/CoexistencePaper")

#Different values of the fitness cost parameter 
cvalues <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.1, 0.15, 0.25)

set.seed(30)
mycolorsMathFigures <- sample(colorRampPalette(brewer.pal(8, "Set2"))(9))

segment_df <- data.frame(
  cvalues = cvalues,
  xend = cvalues + 0.01,
  y = rep(.995, length(cvalues)), ##0.995 instead of 1 for visibility in the image
  yend = rep(0.995, length(cvalues)),
  color = mycolorsMathFigures[1:9] # Store colors here
)

png(filename = "Figures/Figure2_EvolutionVsFractionResistant.png", width = 5, height = 4, units = "in", res = 300)

ggplot() +
  # Add ablines with slopes 1/cvalues because f/te = c at equilibrium 
  geom_abline(intercept = 0, slope = 1/cvalues, linetype = 1, 
              color = mycolorsMathFigures[1:9], linewidth = 1) +
  # Add segments
  geom_segment(data = segment_df, 
               aes(x = cvalues, y = y, xend = xend, yend = yend, color = as.factor(cvalues)), 
               linewidth = 1) + 
  # Manually set colors
  scale_color_manual(values = mycolorsMathFigures[1:9]) +
  # Labels and themes
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  labs(y = "Predicted fraction resistant", 
       x = "Rate of evolution (t*e)", 
       color = "Cost of resistance") +
  # Axis scaling
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.05)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.003))

dev.off()







