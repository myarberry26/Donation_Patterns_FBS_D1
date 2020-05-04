# Final Project 

library(rvest)
library(readr)
library(DT)
library(tidyverse)

# Number of National Championship by School 
NCAA <- read_html("https://en.wikipedia.org/wiki/College_football_national_championships_in_NCAA_Division_I_FBS") 
NatChap <- NCAA %>% html_nodes("#mw-content-text > div > table:nth-child(60)") %>%
  html_table(fill=TRUE) 
FootballTable <- NatChap[[1]]
FootballTable[4] <- NULL
FootballTable[6, 2] = 11
FootballTable[33, 2] = 2
FootballTable[36, 2] = 2
FootballTable[13, 1] <- "Miami"

#Finances for 2017 - 2018 
Finances2018 <- read_html("https://sports.usatoday.com/ncaa/finances/") 
NCAAFinances  <- Finances2018 %>% html_nodes("#content > div.full-width > div > section > div.datatable-wrapper.datatable-wrapper-fixed-column > table") %>%
  html_table(fill=TRUE) 
NCAAFinances2018 <- NCAAFinances[[1]]
NCAAFinances2018 = NCAAFinances2018[-231,]
NCAAFinances2018$`Total Revenue` <- parse_number(NCAAFinances2018$`Total Revenue`)
NCAAFinances2018$`Total Expenses` <- parse_number(NCAAFinances2018$`Total Expenses`)
NCAAFinances2018$`Total Allocated` <- parse_number(NCAAFinances2018$`Total Allocated`)

# Head Coach Salaries 
Salary <- read_html("https://sports.usatoday.com/ncaa/salaries/") 
HeadCoachSalary  <- Salary %>% html_nodes("#content > div.full-width > div > section > div.datatable-wrapper.datatable-wrapper-fixed-column > table") %>%
  html_table(fill=TRUE) 
HeadCoachSalary <- HeadCoachSalary[[1]]
is.na(HeadCoachSalary) <- HeadCoachSalary == "--"
HeadCoachSalary = HeadCoachSalary[-131,]
HeadCoachSalary = HeadCoachSalary[-1]
HeadCoachSalary = HeadCoachSalary[-2]
HeadCoachSalary = HeadCoachSalary[-8]
HeadCoachSalary = HeadCoachSalary[-6]
HeadCoachSalary$`School Pay` <- parse_number(HeadCoachSalary$`School Pay`)
HeadCoachSalary$`Total Pay` <- parse_number(HeadCoachSalary$`Total Pay`)
HeadCoachSalary$`Asst Pay Total` <- parse_number(HeadCoachSalary$`Asst Pay Total`)

# 2019 Winning percentage 
library(readxl)
WinPCT19 <- read_excel("~/Data Science for Econometics/WinPCT.xlsx")

# 2018 Winning percentage 
library(readxl)
WinPCT18 <- read_excel("~/Data Science for Econometics/WinPCT18.xlsx")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#2019 Winning Percentage 
NCAAD1Football <- merge(NCAAFinances2018,HeadCoachSalary,by="School")
NCAAD1Football = NCAAD1Football[-2]
NCAAD1Football <- merge(NCAAD1Football,WinPCT19,by="School")
names(NCAAD1Football)[3] <- "Total.Revenue"
names(NCAAD1Football)[4] <- "Total.Expenses"
names(NCAAD1Football)[5] <- "Total.Allocated"
names(NCAAD1Football)[6] <- "%.Allocated"
names(NCAAD1Football)[8] <- "School.Pay"
names(NCAAD1Football)[9] <- "Total.Pay"
names(NCAAD1Football)[10] <- "Max.Bonas"
names(NCAAD1Football)[11] <- "Asst.Pay.Total"


# The 2018 Season Winning Percentage 
NCAAD1Football <- merge(NCAAD1Football,WinPCT18,by="School")

NCAAD1Football$Total.Donation <- NCAAD1Football$Total.Revenue * .24
NCAAD1Football$Allocated.Donation <- NCAAD1Football$Total.Allocated * .24

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
WinImpact19 <- lm(PCT.19 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact19)
library(stargazer)
stargazer(WinImpact19)

WinImpact18 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact18)

# only the power five conferences 
Power5 <- NCAAD1Football[which(NCAAD1Football$Conf == 'Big Ten' | NCAAD1Football$Conf == 'Big 12' | NCAAD1Football$Conf == 'SEC' | NCAAD1Football$Conf == 'Pac-12' | NCAAD1Football$Conf == 'ACC'),]
P5WinImpact <- lm(PCT.19 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact)

P5WinImpact2018 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact2018)

# None Power Five Schools 
NotPower5 <- NCAAD1Football[which(NCAAD1Football$Conf != 'Big Ten' | NCAAD1Football$Conf != 'Big 12' | NCAAD1Football$Conf != 'SEC' | NCAAD1Football$Conf != 'Pac-12' | NCAAD1Football$Conf != 'ACC'),]
NotP5WinImpact <- lm(PCT.19 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact)

NotP5WinImpact2018 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact2018)
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Total Expenses Impact 
WinImpact19 <- lm(PCT.19 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact19)
library(stargazer)
stargazer(WinImpact19)

WinImpact18 <- lm(PCT.18 ~ Total.Revenue + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(WinImpact18)

# only the power five conferences 
Power5 <- NCAAD1Football[which(NCAAD1Football$Conf == 'Big Ten' | NCAAD1Football$Conf == 'Big 12' | NCAAD1Football$Conf == 'SEC' | NCAAD1Football$Conf == 'Pac-12' | NCAAD1Football$Conf == 'ACC'),]
P5WinImpact <- lm(PCT.19 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact)

P5WinImpact2018 <- lm(PCT.18 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5WinImpact2018)

# None Power Five Schools 
NotPower5 <- NCAAD1Football[which(NCAAD1Football$Conf != 'Big Ten' | NCAAD1Football$Conf != 'Big 12' | NCAAD1Football$Conf != 'SEC' | NCAAD1Football$Conf != 'Pac-12' | NCAAD1Football$Conf != 'ACC'),]
NotP5WinImpact <- lm(PCT.19 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact)

NotP5WinImpact2018 <- lm(PCT.18 ~ Total.Expenses + Total.Allocated + Total.Pay + Asst.Pay.Total, data = NotPower5)
summary(NotP5WinImpact2018)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Impact of Donation on Winning 
DonationImpact18 <- lm(PCT.18 ~ Total.Donation + Allocated.Donation + Total.Pay + Asst.Pay.Total, data = NCAAD1Football)
summary(DonationImpact18)

P5DonationImpact18 <- lm(PCT.18 ~ Total.Donation + Allocated.Donation + Total.Pay + Asst.Pay.Total, data = Power5)
summary(P5DonationImpact18 )

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
NCAAFinances2018$`Total Revenue` <- parse_number(NCAAFinances2018$`Total Revenue`)
NCAAFinances2018$`Total Expenses` <- parse_number(NCAAFinances2018$`Total Expenses`)
NCAAFinances2018$`Total Allocated` <- parse_number(NCAAFinances2018$`Total Allocated`)
AverageConf <- aggregate(NCAAFinances2018[, c(4:6)], list(NCAAFinances2018$Conf), mean)

ggplot() + 
  geom_boxplot(data = NCAAFinances2018, aes(y = NCAAFinances2018$`Total Revenue`, x = as.factor(NCAAFinances2018$Conf)), fill = "light blue") +
  coord_flip() +
  theme_bw() +
  ggtitle("Total Athletic Depatment Revenue based on Conference") +
  ylab("Total Funds") +
  xlab("Conferences") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
