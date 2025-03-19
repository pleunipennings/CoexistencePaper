
#OK in this notebook, I will plot the change in resistance and the change in usage for 
#20 European countries between 2011 and 2022. 
# In this case for betalactam resistance in Staph aureus 
# The UK data stop at 2020, so I plot 2011-2019 instead. 

setwd("~/Documents/GitHub/CoexistencePaper")
library(ggplot2)
library(tidyverse)

## Before running this script, I need to run the data prep script 
## R_Europe_BetaLactamsTimeSeriesPrepareData.R

## Read the data and keep only the 20 largest EU countries (including the UK)
BetaLactamData <- read.csv("ECDCData/BetaLactamSAureusResistanceAndUsageEurope_Feb2025.csv")
MediumCountries <- c("Germany", "United Kingdom", "France",
                     "Italy" , "Spain" ,   "Poland","Romania", "Netherlands","Belgium" ,"Czechia", "Sweden",
                     "Portugal","Greece","Hungary","Austria", "Bulgaria","Denmark","Finland","Slovakia", "Ireland")
BetaLactamData<- BetaLactamData[BetaLactamData$Country.name %in% MediumCountries,]

#Create new columns to record the change in one year. 
BetaLactamData$ChangeUsage1Year <- NA
BetaLactamData$ChangeResistance1Year <- NA

for (i in 1:nrow(BetaLactamData)){
  CountryName = BetaLactamData$Country.name[i]
  Year = BetaLactamData$Year[i]
  print(paste0(CountryName, Year))
  #check if previous year exists. 
  if (length(which(BetaLactamData$Year == Year - 1 & BetaLactamData$Country.name == CountryName)) == 1){ 
    print("previous year exists") 
    prevRow = which(BetaLactamData$Year == Year - 1 & BetaLactamData$Country.name == CountryName)
    BetaLactamData$ChangeResistance1Year[i] = BetaLactamData$ResistanceLevel[i] - BetaLactamData$ResistanceLevel[prevRow]
    BetaLactamData$ChangeUsage1Year[i] = BetaLactamData$Usage[i] - BetaLactamData$Usage[prevRow]
  }
}

#Make colors (thanks to Olivia Pham)
color_hex <- c('#e6194b', '#3cb44b', '#00ff00', '#4363d8', 
               '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
               '#bcf60c', '#cccccc', '#008080', '#e6beff', 
               '#9a6324', '#2f4f4f', '#800000', '#57cc99', 
               '#808000', '#ff1493', '#000075', '#808080')

# Make a data structure with country names and colors
country_colors <- data.frame(Country.name = MediumCountries) %>%
  distinct()
country_colors <- cbind(country_colors, color_hex)
ordered_color <- country_colors %>% pull(color_hex) 
names(ordered_color) <- country_colors %>% pull(Country.name) 

## Calculate the R2 for usage change vs resistance change 
Rsqvalue1Year = cor.test(BetaLactamData$ChangeUsage1Year, BetaLactamData$ChangeResistance1Year)$estimate^2
Rsqvalue1Year = round(Rsqvalue1Year,2)

#Plot the effect of one year change
png(filename = "Figures/BetaLactamFigures/ChangeInResistanceAndUsage1YearEurope.png", width = 7, height = 8, units = "in", res = 300)
ggplot(BetaLactamData, aes(x = ChangeUsage1Year, y = ChangeResistance1Year, label = Country.name)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey") +  # Vertical line at x = 0
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey") + # Horizontal line at y = 0
  geom_smooth(method = "lm",  color = "grey", linewidth = 2, alpha = 0.5, se = FALSE) +  # Regression line
  geom_point(aes(color = Country.name, size = log(SampleSize)), alpha = 0.5) +  # Scatter plot points
  scale_size(range = c(0, 5))+
  scale_color_manual(values = ordered_color)+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,
                                    color="black", 
                                    size=1.7, 
                                    linetype="solid"),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  labs(x = "Change in usage (DDD/1000 from year to year)", 
       y = "Change in Resistance (% from year to year)", 
       title = "Change in BetaLactam Usage vs. Resistance") +
  annotate(geom="text", x=-1.8, y=10, label=paste("R^2 =", Rsqvalue1Year), size = 5)+
  xlim(-3,2)+
  ylim(-15,15)
dev.off()  

## Now let's look at longer intervals. Test for 2017 - 2011 and 2022 - 2017
## Is the effect stronger? 
BetaLactamData$ChangeUsage5Years <- NA
BetaLactamData$ChangeResistance5Years <- NA

for (i in 1:nrow(BetaLactamData)){
  CountryName = BetaLactamData$Country.name[i]
  Year = BetaLactamData$Year[i]
  print(paste0(CountryName, Year))
  #check if 2017. 
  if (Year == 2017){
    if (length(which(BetaLactamData$Year == 2011 & BetaLactamData$Country.name == CountryName))==1){
      print("2017 and 2011 exists")
      Row11 = which(BetaLactamData$Year == 2011& BetaLactamData$Country.name == CountryName)
      BetaLactamData$ChangeResistance5Years[i] = BetaLactamData$ResistanceLevel[i] - BetaLactamData$ResistanceLevel[Row11]
      BetaLactamData$ChangeUsage5Years[i] = BetaLactamData$Usage[i] - BetaLactamData$Usage[Row11]
    }
  }
  if (Year == 2022){
    if (length(which(BetaLactamData$Year == 2017 & BetaLactamData$Country.name == CountryName))==1){
      print("2017 and 2011 exists")
      Row17 = which(BetaLactamData$Year == 2017& BetaLactamData$Country.name == CountryName)
      BetaLactamData$ChangeResistance5Years[i] = BetaLactamData$ResistanceLevel[i] - BetaLactamData$ResistanceLevel[Row17]
      BetaLactamData$ChangeUsage5Years[i] = BetaLactamData$Usage[i] - BetaLactamData$Usage[Row17]
    }
  }
}

## Calculate the R2 for usage change vs resistance change 
Rsqvalue5Years = cor.test(BetaLactamData$ChangeUsage5Years, BetaLactamData$ChangeResistance5Years)$estimate^2
Rsqvalue5Years = round(Rsqvalue5Years,2)

#Plot the effect of 5 years change
png(filename = "Figures/BetaLactamFigures/ChangeInResistanceAndUsage5YearsEurope.png", width = 7, height = 8, units = "in", res = 300)
ggplot(BetaLactamData, aes(x = ChangeUsage5Years, y = ChangeResistance5Years, label = Country.name)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey") +  # Vertical line at x = 0
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey") + # Horizontal line at y = 0
  geom_smooth(method = "lm",  color = "grey", linewidth = 2, alpha = 0.5, se = FALSE) +  # Regression line
  geom_point(aes(color = Country.name, size = log(SampleSize)), alpha = 0.5) +  # Scatter plot points
  scale_size(range = c(0, 5))+
  scale_color_manual(values = ordered_color)+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,
                                    color="black", 
                                    size=1.7, 
                                    linetype="solid"),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  labs(x = "Change in usage (DDD/1000 over 5 years)", 
       y = "Change in Resistance (% over 5 years)", 
       title = "Change in Betalactam Usage vs. Resistance") +
  annotate(geom="text", x=-1.8, y=10, label=paste("R^2 =", Rsqvalue5Years), size = 5)+
  xlim(-3,2)+
  ylim(-15,15)

dev.off()  
  

## Now let's test for the entire time span we have: 2022 - 2011
## For the UK, we test 2019 - 2011. The UK is interesting because there is virtually no change in the UK. 

BetaLactamData$ChangeUsageTotal <- NA
BetaLactamData$ChangeResistanceTotal <- NA

for (CountryName in MediumCountries){
  LastYear <- max(BetaLactamData$Year[BetaLactamData$Country.name==CountryName & !is.na(BetaLactamData$Usage)])
  FirstYear <- min(BetaLactamData$Year[BetaLactamData$Country.name==CountryName & !is.na(BetaLactamData$Usage)])
  print(paste0(CountryName, " ", FirstYear, " ", LastYear))
  lastrow = which(BetaLactamData$Country.name==CountryName & BetaLactamData$Year == LastYear)
  firstrow = which(BetaLactamData$Country.name==CountryName & BetaLactamData$Year == FirstYear)
  BetaLactamData$ChangeResistanceTotal[lastrow] = BetaLactamData$ResistanceLevel[lastrow] - BetaLactamData$ResistanceLevel[firstrow]
  BetaLactamData$ChangeUsageTotal[lastrow] = BetaLactamData$Usage[lastrow] - BetaLactamData$Usage[firstrow]
}

## Calculate the R2 for usage change vs resistance change 
Rsqvalue = cor.test(BetaLactamData$ChangeUsageTotal, BetaLactamData$ChangeResistanceTotal)$estimate^2
Rsqvalue = round(Rsqvalue,2)

#Plot the effect of 11 years change
png(filename = "Figures/BetaLactamFigures/ChangeInResistanceAndUsageEurope11Years.png", width = 7, height = 8, units = "in", res = 300)
ggplot(BetaLactamData, aes(x = ChangeUsageTotal, y = ChangeResistanceTotal, label = Country.name)) +
  #geom_abline(intercept = 0, slope = 8.430, color = "grey", linetype = "dashed", size = 1.2) +  # Custom slope line
  geom_vline(xintercept = 0, linetype = "solid", color = "darkgrey") +  # Vertical line at x = 0
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey") + # Horizontal line at y = 0
  geom_smooth(method = "lm",  color = "grey", linewidth = 2, alpha = 0.5, se = FALSE) +  # Regression line
  geom_point(aes(color = Country.name), size = 3) +  # Scatter plot points
  scale_color_manual(values = ordered_color)+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,
                                    color="black", 
                                    size=1.7, 
                                    linetype="solid"),
        text = element_text(size=16),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  labs(x = "Change in usage (DDD/1000 between 2011 and 2022)", 
       y = "Change in Resistance (% between 2011 and 2022)", 
       title = "Change in BetaLactam Usage vs. Resistance") +
  #theme_minimal()+
  geom_text_repel(nudge_x = .3)+
  annotate(geom="text", x=-1.8, y=10, label=paste("R^2 =", Rsqvalue), size = 5)
  #xlim(-3,2)+
  #ylim(-15,15)

dev.off()  

## Focus only on the rows that have the total change information (last row for each country)
BetaLactamData_Change <- BetaLactamData[!is.na(BetaLactamData$ChangeResistanceTotal),]

# Split the countries in three categories for plotting purposes
BetaLactamData_Change$Category <- "Usage down, Resistance down"
BetaLactamData_Change$Category[BetaLactamData_Change$Country.name %in% c("Sweden", "Denmark", "Netherlands", "Spain")] <- c("Usage down, Resistance up")
BetaLactamData_Change$Category[BetaLactamData_Change$Country.name %in% c("Germany")] <- c("Other")
#BetaLactamData_Change$Category[BetaLactamData_Change$Country.name %in% c("United Kingdom")] <- c("Other")
BetaLactamData_Change$Category <- factor(BetaLactamData_Change$Category, levels = c("Usage down, Resistance down", "Usage down, Resistance up", "Other")) 

## Now create figure for paper with arrows and countries split in three categories
## make list of three plots
ggList <- lapply(split(BetaLactamData_Change, BetaLactamData_Change$Category), function(i) {
  ggplot(i) +
    # Add the shifted points
    geom_point(aes(color = Country.name, x = Usage - ChangeUsageTotal, y = ResistanceLevel - ChangeResistanceTotal), 
               size = 3, alpha = 0.5) +
    scale_color_manual(values = ordered_color)+
    # Draw arrows from the shifted points to the original points
    theme(legend.position = "bottom",legend.title = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA,
                                      color="black", 
                                      size=1.7, 
                                      linetype="solid"),
          text = element_text(size=16),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10)),
          plot.margin = margin(.5, .8, .5, .8, "cm"))+
    geom_segment(aes(color = Country.name, x = Usage - ChangeUsageTotal, 
                     y = ResistanceLevel - ChangeResistanceTotal, 
                     xend = Usage, 
                     yend = ResistanceLevel), 
                 arrow = arrow(length = unit(0.1, "inches")), linewidth = 1.7, alpha = 0.9) +
    labs(x = "BetaLactam Usage DDD/1000", 
         y = "Percentage BetaLactam-resistant isolates", 
         title = "Usage and Resistance 2011-2022") +
    xlim(0,18)+
    ylim(0,60)+
    theme(axis.title.y = element_text(margin = margin(r = 5)))
  })

# plot 1 (with 15 countries) will be biggest
X = cowplot::plot_grid(plotlist = ggList[1], ncol = 1,
                   align = 'hv', labels = "a.", label_size = 30)
# plot 2 and 3 (with 3 and 2 countries) will be smalller and above each other 
Y = cowplot::plot_grid(plotlist = ggList[2:3], ncol = 1,
                       align = 'hv', 
                       labels = c("b.","c."), label_size = 30, label_x = 0, hjust = 0.5)

## Put the three plots together in one png file 
png(filename = "Figures/BetaLactamFigures/ChangeInResistanceAndUsage11YearsEurope.png", width = 13, height = 10, units = "in", res = 300)
cowplot::plot_grid(X, Y, rel_widths = c(3,2))
dev.off()

