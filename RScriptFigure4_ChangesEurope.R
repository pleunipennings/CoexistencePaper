
#OK in this notebook, I will plot the change in resistance and the change in usage for 
#20 European countries between 2011 and 2022. 
# The UK data stop at 2020, so I plot 2011-2019 instead. 

setwd("~/Documents/GitHub/CoexistencePaper")
library(ggplot2)
library(tidyverse)

## Before running this script, I need to run the data prep script 
## R_Europe_QuinolonesTimeSeriesPrepareData.R

## Read the data and keep only the 20 largest EU countries (including the UK)
Quin_Data <- read.csv("ECDCData/QuinoloneResistanceAndUsageEurope_Feb2025.csv")
MediumCountries <- c("Germany", "United Kingdom", "France",
                     "Italy" , "Spain" ,   "Poland","Romania", "Netherlands","Belgium" ,"Czechia", "Sweden",
                     "Portugal","Greece","Hungary","Austria", "Bulgaria","Denmark","Finland","Slovakia", "Ireland")
Quin_Data<- Quin_Data[Quin_Data$Country.name %in% MediumCountries,]

#Create new columns to record the change in one year. 
Quin_Data$ChangeUsage1Year <- NA
Quin_Data$ChangeResistance1Year <- NA

for (i in 1:nrow(Quin_Data)){
  CountryName = Quin_Data$Country.name[i]
  Year = Quin_Data$Year[i]
  print(paste0(CountryName, Year))
  #check if previous year exists. 
  if (length(which(Quin_Data$Year == Year - 1 & Quin_Data$Country.name == CountryName)) == 1){ 
    print("previous year exists") 
    prevRow = which(Quin_Data$Year == Year - 1 & Quin_Data$Country.name == CountryName)
    Quin_Data$ChangeResistance1Year[i] = Quin_Data$ResistanceLevel[i] - Quin_Data$ResistanceLevel[prevRow]
    Quin_Data$ChangeUsage1Year[i] = Quin_Data$Usage[i] - Quin_Data$Usage[prevRow]
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
Rsqvalue1Year = cor.test(Quin_Data$ChangeUsage1Year, Quin_Data$ChangeResistance1Year)$estimate^2
Rsqvalue1Year = round(Rsqvalue1Year,2)

#Plot the effect of one year change
png(filename = "Figures/AdditionalTimeSeriesFigures/ChangeInResistanceAndUsage1YearEurope.png", width = 7, height = 8, units = "in", res = 300)
ggplot(Quin_Data, aes(x = ChangeUsage1Year, y = ChangeResistance1Year, label = Country.name)) +
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
       title = "Change in Quinolone Usage vs. Resistance") +
  annotate(geom="text", x=-1.8, y=10, label=paste("R^2 =", Rsqvalue1Year), size = 5)+
  xlim(-3,2)+
  ylim(-15,15)
dev.off()  

## Now let's look at longer intervals. Test for 2017 - 2011 and 2022 - 2017
## Is the effect stronger? 
Quin_Data$ChangeUsage5Years <- NA
Quin_Data$ChangeResistance5Years <- NA

for (i in 1:nrow(Quin_Data)){
  CountryName = Quin_Data$Country.name[i]
  Year = Quin_Data$Year[i]
  print(paste0(CountryName, Year))
  #check if 2017. 
  if (Year == 2017){
    if (length(which(Quin_Data$Year == 2011 & Quin_Data$Country.name == CountryName))==1){
      print("2017 and 2011 exists")
      Row11 = which(Quin_Data$Year == 2011& Quin_Data$Country.name == CountryName)
      Quin_Data$ChangeResistance5Years[i] = Quin_Data$ResistanceLevel[i] - Quin_Data$ResistanceLevel[Row11]
      Quin_Data$ChangeUsage5Years[i] = Quin_Data$Usage[i] - Quin_Data$Usage[Row11]
    }
  }
  if (Year == 2022){
    if (length(which(Quin_Data$Year == 2017 & Quin_Data$Country.name == CountryName))==1){
      print("2017 and 2011 exists")
      Row17 = which(Quin_Data$Year == 2017& Quin_Data$Country.name == CountryName)
      Quin_Data$ChangeResistance5Years[i] = Quin_Data$ResistanceLevel[i] - Quin_Data$ResistanceLevel[Row17]
      Quin_Data$ChangeUsage5Years[i] = Quin_Data$Usage[i] - Quin_Data$Usage[Row17]
    }
  }
}

## Calculate the R2 for usage change vs resistance change 
Rsqvalue5Years = cor.test(Quin_Data$ChangeUsage5Years, Quin_Data$ChangeResistance5Years)$estimate^2
Rsqvalue5Years = round(Rsqvalue5Years,2)

#Plot the effect of 5 years change
png(filename = "Figures/AdditionalTimeSeriesFigures/ChangeInResistanceAndUsage5YearsEurope.png", width = 7, height = 8, units = "in", res = 300)
ggplot(Quin_Data, aes(x = ChangeUsage5Years, y = ChangeResistance5Years, label = Country.name)) +
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
       title = "Change in Quinolone Usage vs. Resistance") +
  annotate(geom="text", x=-1.8, y=10, label=paste("R^2 =", Rsqvalue5Years), size = 5)+
  xlim(-3,2)+
  ylim(-15,15)

dev.off()  
  

## Now let's test for the entire time span we have: 2022 - 2011
## For the UK, we test 2019 - 2011. The UK is interesting because there is virtually no change in the UK. 

Quin_Data$ChangeUsageTotal <- NA
Quin_Data$ChangeResistanceTotal <- NA

for (CountryName in MediumCountries){
  LastYear <- max(Quin_Data$Year[Quin_Data$Country.name==CountryName & !is.na(Quin_Data$Usage)])
  FirstYear <- min(Quin_Data$Year[Quin_Data$Country.name==CountryName & !is.na(Quin_Data$Usage)])
  print(paste0(CountryName, " ", FirstYear, " ", LastYear))
  lastrow = which(Quin_Data$Country.name==CountryName & Quin_Data$Year == LastYear)
  firstrow = which(Quin_Data$Country.name==CountryName & Quin_Data$Year == FirstYear)
  Quin_Data$ChangeResistanceTotal[lastrow] = Quin_Data$ResistanceLevel[lastrow] - Quin_Data$ResistanceLevel[firstrow]
  Quin_Data$ChangeUsageTotal[lastrow] = Quin_Data$Usage[lastrow] - Quin_Data$Usage[firstrow]
}

## Calculate the R2 for usage change vs resistance change 
Rsqvalue = cor.test(Quin_Data$ChangeUsageTotal, Quin_Data$ChangeResistanceTotal)$estimate^2
Rsqvalue = round(Rsqvalue,2)

#Plot the effect of 11 years change
png(filename = "Figures/AdditionalTimeSeriesFigures/ChangeInResistanceAndUsageEurope11Years.png", width = 7, height = 8, units = "in", res = 300)
ggplot(Quin_Data, aes(x = ChangeUsageTotal, y = ChangeResistanceTotal, label = Country.name)) +
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
       title = "Change in Quinolone Usage vs. Resistance") +
  #theme_minimal()+
  geom_text_repel(nudge_x = .3)+
  annotate(geom="text", x=-1.8, y=10, label=paste("R^2 =", Rsqvalue), size = 5)+
  xlim(-3,2)+
  ylim(-15,15)

dev.off()  

## Focus only on the rows that have the total change information (last row for each country)
Quin_Data_Change <- Quin_Data[!is.na(Quin_Data$ChangeResistanceTotal),]

# Split the countries in three categories for plotting purposes
Quin_Data_Change$Category <- "Usage down, Resistance down"
Quin_Data_Change$Category[Quin_Data_Change$Country.name %in% c("Poland", "Bulgaria", "Greece")] <- c("Usage up, Resistance up")
Quin_Data_Change$Category[Quin_Data_Change$Country.name %in% c("Sweden")] <- c("Other")
Quin_Data_Change$Category[Quin_Data_Change$Country.name %in% c("United Kingdom")] <- c("Other")
Quin_Data_Change$Category <- factor(Quin_Data_Change$Category, levels = c("Usage down, Resistance down", "Usage up, Resistance up", "Other")) 

## Now create figure for paper with arrows and countries split in three categories
## make list of three plots
ggList <- lapply(split(Quin_Data_Change, Quin_Data_Change$Category), function(i) {
  ggplot(i) +
    geom_abline(intercept = 10, slope = 8.430, color = "grey", linetype = "solid", size = 2, alpha = 0.5) + 
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
    labs(x = "Quinolone Usage DDD/1000", 
         y = "Percentage quinolone-resistant isolates", 
         title = "Usage and Resistance 2011-2022") +
    xlim(0,4.)+
    ylim(5,45)+
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
png(filename = "Figures/Figure4ChangeInResistanceAndUsage11YearsEurope.png", width = 13, height = 10, units = "in", res = 300)
cowplot::plot_grid(X, Y, rel_widths = c(3,2))
dev.off()

