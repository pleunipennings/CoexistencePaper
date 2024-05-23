#Plot the European CSC data
setwd("~/Documents/GitHub/CoexistencePaper")
library(ggplot2)
library(ggrepel)
library(Polychrome)
library(here)
library(tidyverse)

ECDCData <- read.csv("ECDC_surveillance_data_Antimicrobial_resistance.csv", 
                     stringsAsFactors = FALSE)
ECDCData <- ECDCData[ECDCData$Indicator == "R - resistant isolates, percentage  ",]
ECDCData$NumValue <- as.numeric(as.character(ECDCData$NumValue))
summary(ECDCData)
ECDCData$RegionName<-as.factor(ECDCData$RegionName)

ggplot(data = ECDCData, mapping = aes(x = Time, y = NumValue))+
  geom_line(aes(color = RegionName))+
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0,100))

BigCountries <- c("Germany", 
"United Kingdom",
"France",
"Italy" ,
"Spain" ,
"Poland",
"Romania",
"Netherlands",
"Belgium" ,
"Czechia")

ECDCDataBigCountries <- ECDCData[ECDCData$RegionName %in% BigCountries,]

MediumCountries <- c("Germany", 
                  "United Kingdom",
                  "France",
                  "Italy" ,
                  "Spain" ,
                  "Poland",
                  "Romania",
                  "Netherlands",
                  "Belgium" ,
                  "Czechia", 
                  "Sweden",
                  "Portugal",
                  "Greece",
                  "Hungary",
                  "Austria",
                  "Bulgaria",
                  "Denmark",
                  "Finland",
                  "Slovakia",
                  "Ireland"
                  )

QuinUseTable<-read.csv("2015_data_Table_D6_J01M_quinolone antibacterials_trend_community_sparklines.csv")
QuinoloneUse<-c()
for (co in MediumCountries){
  if(length(which(QuinUseTable$Country==co))==1){
    QuinoloneUse<-c(QuinoloneUse,QuinUseTable$X2015[which(QuinUseTable$Country==co)])}
  else{QuinoloneUse<-c(QuinoloneUse,NA)}
}

Resistance2015 <- c()
for (c in 1:length(MediumCountries)){
  print(MediumCountries[c])
  Resistance2015 <-  c(Resistance2015, ECDCData$NumValue
                       [ECDCData$Time==2015 & ECDCData$RegionName == MediumCountries[c]])
}

QuinUseDF <- data.frame(MediumCountries, QuinoloneUse, Resistance2015)
QuinUseDF <- QuinUseDF[!is.na(QuinUseDF$QuinoloneUse),]

#Make colors (thanks to Olivia Pham)
color_hex <- c('#e6194b', '#3cb44b', '#00ff00', '#4363d8', 
               '#f58231', '#911eb4', '#46f0f0', '#f032e6', 
               '#bcf60c', '#cccccc', '#008080', '#e6beff', 
               '#9a6324', '#2f4f4f', '#800000', '#57cc99', 
               '#808000', '#ff1493', '#000075', '#808080')


# Combine the countries
country_colors <- data.frame(RegionName =c(BigCountries, MediumCountries)) %>%
  distinct()

country_colors <- cbind(country_colors, color_hex)

ordered_color <- country_colors %>% pull(color_hex) 
names(ordered_color) <- country_colors %>% pull(RegionName) 
ordered_color

png(filename = "Figure1A_ECDCPlot_ResistanceOverTime.png", width = 7, height = 3.8, units = "in", res = 300)
ggplot(data = ECDCDataBigCountries, mapping = aes(x = Time, y = NumValue, color = RegionName))+
  geom_line(aes(color = RegionName))+
  scale_color_manual(values = ordered_color)+
  scale_y_continuous(limits = c(0,100))+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,
                                    color="black", 
                                    size=1.2, 
                                    linetype="solid")) +
  labs(y="Percentage resistant isolates", 
       x = "Year",
       color = "Region Name")
dev.off()

Rsqvalue = as.character(round(cor.test(QuinoloneUse, Resistance2015)$estimate,2))

png(filename = "Figure1B_ECDCPlot_Correlation.png", width = 5, height = 6, units = "in", res = 300)

ggplot(data = QuinUseDF, mapping = aes(x = QuinoloneUse, y = Resistance2015, color = MediumCountries, label=MediumCountries))+
  geom_point(aes(color = MediumCountries), size = 3, show.legend = FALSE)+
  scale_color_manual(values = ordered_color)+
  scale_y_continuous(limits = c(7,50))+
  scale_x_continuous(limits = c(0,4.5))+
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
  labs(y= "Percentage resistant isolates 2015", x = "Quinolone Use DDD / 1000")+
  
  annotate(geom="text", x=1, y=40, label=paste("R^2 =", Rsqvalue), size = 5)+
  geom_text_repel(nudge_x = .3)+
  geom_abline(slope = lm(Resistance2015 ~ QuinoloneUse)$coefficients[2], intercept = lm(Resistance2015 ~ QuinoloneUse)$coefficients[1], 
              color = "grey", linewidth = 2, alpha = 0.5) 

dev.off()

cor.test(QuinoloneUse, Resistance2015)
summary(lm(Resistance2015 ~ QuinoloneUse))
