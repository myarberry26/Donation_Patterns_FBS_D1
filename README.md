# Donation_Patterns_FBS_D1
Analyzing Athletic Donation Patterns in Donors at FBS Division 1 Universities using R for the 2018-2019 season
Final Project Spring 2020 

Getting Started
There is a csv file that will need to be downloaded under the Data folder 
    you will need to download the CSV titled NCAAD1Football. The rest of the data sets are scraped using web scraping procedures through the program R Studio. 

Importing Data
After downloading the csv you will not need to scrape any addtional data all collection was done and exported on file for convience
~~~~~~
#reading in Data 
NCAAD1Football <- read.csv(file = "C:\\Users\\myarb\\Downloads\\NCAAD1Football.csv", header = TRUE, sep = ",")
~~~~~~

Regression Analysis for Total Revenue

~~~~~~
#Regression for the 2019 Season
library(tidyverse)
WinImpact19 <- lm(PCT.19 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact19)

#Regression for the 2018 Season 
WinImpact18 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact18)

# Seperation of the Teams in the Power 5 
Power5 <- NCAAD1Football[which(NCAAD1Football$Conf == 'Big Ten' | NCAAD1Football$Conf == 'Big 12' | NCAAD1Football$Conf == 'SEC' | NCAAD1Football$Conf == 'Pac-12' | NCAAD1Football$Conf == 'ACC'),]

NotPower5 <- NCAAD1Football[which(NCAAD1Football$Conf != 'Big Ten' | NCAAD1Football$Conf != 'Big 12' | NCAAD1Football$Conf != 'SEC' | NCAAD1Football$Conf != 'Pac-12' | NCAAD1Football$Conf != 'ACC'),]

# Rerunning the Regression 
P5WinImpact <- lm(PCT.19 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact)
P5WinImpact2018 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact2018)

# Those not in the Power 5 Conferences 
NotP5WinImpact <- lm(PCT.19 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact)
NotP5WinImpact2018 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact2018)
~~~~~~

Regression Analysis for Total Expenses

~~~~~~
# Total Expenses Impact 
WinImpact19 <- lm(PCT.19 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact19)
library(stargazer)
stargazer(WinImpact19)

WinImpact18 <- lm(PCT.18 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact18)

# only the power five conferences 
P5WinImpact <- lm(PCT.19 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact)

P5WinImpact2018 <- lm(PCT.18 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact2018)

# None Power Five Schools 
NotP5WinImpact <- lm(PCT.19 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact)

NotP5WinImpact2018 <- lm(PCT.18 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact2018)
~~~~~~

Impact of Donation

~~~~~~
# Impact of Donation on Winning 
DonationImpact18 <- lm(PCT.18 ~ Total.Donation + Allocated.Donation + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(DonationImpact18)

P5DonationImpact18 <- lm(PCT.18 ~ Total.Donation + Allocated.Donation + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5DonationImpact18 )
~~~~~~

Visualization

~~~~~~
#Box and Wisker plot of Total Revenue 
AverageConf <- aggregate(NCAAFinances2018[, c(4:6)], list(NCAAFinances2018$Conf), mean)

ggplot() + 
  geom_boxplot(data = NCAAFinances2018, aes(y = NCAAFinances2018$`Total Revenue`, x = as.factor(NCAAFinances2018$Conf)), fill = "light blue") +
  coord_flip() +
  theme_bw() +
  ggtitle("Total Athletic Depatment Revenue based on Conference") +
  ylab("Total Funds") +
  xlab("Conferences") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
~~~~~~

