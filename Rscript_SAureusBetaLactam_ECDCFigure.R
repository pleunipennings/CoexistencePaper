#Plot the European CSC data
setwd("~/Documents/GitHub/CoexistencePaper")
library(ggplot2)
library(ggrepel)
library(Polychrome)
library(here)
library(tidyverse)

## Read the data and keep only the 20 largest EU countries (including the UK)
BetaLactamData <- read.csv("ECDCData/BetaLactamSAureusResistanceAndUsageEurope_Feb2025.csv")
MediumCountries <- c("Germany", "United Kingdom", "France",
                     "Italy" , "Spain" ,   "Poland","Romania", "Netherlands","Belgium" ,"Czechia", "Sweden",
                     "Portugal","Greece","Hungary","Austria", "Bulgaria","Denmark","Finland","Slovakia", "Ireland")
BetaLactamData<- BetaLactamData[BetaLactamData$Country.name %in% MediumCountries,]


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

ggplot(data = BetaLactamData, mapping = aes(x = Year, y = ResistanceLevel))+
  geom_line(aes(color = Country.name))+
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0,100))

UsageBetaLactam2015 = BetaLactamData$Usage[BetaLactamData$Year==2015]
ResistanceBetaLactam2015 = BetaLactamData$ResistanceLevel[BetaLactamData$Year==2015]

Rsqvalue = as.character(round(cor.test(UsageBetaLactam2015, ResistanceBetaLactam2015)$estimate,2)^2)


p = ggplot(data = BetaLactamData[BetaLactamData$Year==2015,], mapping = aes(x = Usage, y = ResistanceLevel, color = Country.name, label=Country.name))+
  geom_point(aes(color = Country.name), size = 3, show.legend = FALSE)+
  scale_color_manual(values = ordered_color)+
  #scale_y_continuous(limits = c(0,50))+
  #scale_x_continuous(limits = c(0,4.5))+
  theme_bw()+
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
  guides(color = guide_legend(title = "Country"))+
  labs(y= "Percentage resistant isolates 2015", x = "Betalactam Use DDD / 1000")+
  annotate(geom="text", x=5, y=50, label=paste("R^2 =", Rsqvalue), size = 5)+
  geom_text_repel(nudge_x = .3)

p+  geom_abline(slope = lm(ResistanceBetaLactam2015 ~ UsageBetaLactam2015)$coefficients[2], intercept = lm(ResistanceBetaLactam2015 ~ UsageBetaLactam2015)$coefficients[1], 
                color = "grey", linewidth = 2, alpha = 0.5)  



pdf ("Figures/BetaLactamFigures/TimeseriesAllCountries_SAureus.pdf")
par(mfrow = c(2,2))
for (CountryName in MediumCountries){
  Subset = BetaLactamData[BetaLactamData$Country.name==CountryName,]
  # Normalize ThirdVar to the range 0-1 if needed
  Subset$YearNormalized <- (Subset$Year - min(Subset$Year)+1) / (max(Subset$Year) - min(Subset$Year)+1)
  # Map Year to grey tones
  colors <- gray(1 - Subset$YearNormalized)  # Reverse to make higher values darker
  #  if ((max(BQ$Value, na.rm = TRUE)-min(BQ$Value, na.rm = TRUE))>0.5){
  plot(Subset$Usage, Subset$ResistanceLevel, type ="b", 
       xlim = c(0,18), ylim = c(0,60), 
       pch = 16, xlab = "BetaLactam usage per 1000 per day",
       ylab = "Percent BetaLactam resistant",
       col = colors, main = CountryName)
  #text(x = 3, y = 37, srt = 33, "linear model from 2015", col= "darkgrey", cex = 0.7)
  #text(x = 3, y = 34, srt = 33, "20 countries", col= "darkgrey", cex = 0.7)
  points(Subset$Usage, Subset$ResistanceLevel, type ="p", pch = 16, cex = 1.2)
  points(Subset$Usage, Subset$ResistanceLevel, type ="l")
  points(Subset$Usage, Subset$ResistanceLevel, type ="p", pch = 16, cex = 1., col = colors)
  #abline(a = 10.062, b=8.430)
  
  legend("topleft", legend = paste("", round(Subset$Year, 2)), 
         pch = 16, col = colors, title = "Year", cex = 0.7)
  
  if (length(which(!is.na(Subset$Usage)))>2 & length(which(!is.na(Subset$ResistanceLevel)))>2){
    R2 = (cor.test(Subset$Usage, Subset$ResistanceLevel)$estimate)^2
    text(x = 15, y = 50, paste("R^2 = ", round(R2,2)))
    Slope = lm(Subset$ResistanceLevel ~ Subset$Usage)$coefficient[2]
  }
}
dev.off()


