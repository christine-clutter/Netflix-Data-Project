
#Master Script
rm(list=ls())
#Webscraping Netflix Data

library(xml2)
page<-read_html("https://www.businessofapps.com/data/netflix-statistics/")
class(page)
    
tables<-xml_find_all(page, "//table[2]")
rows<-xml_find_all(page,"//table[2]//tr/td[1]")
quarters<-xml_text(xml_find_all(page,"//table[2]//tr/td[1]"))
subs<-xml_text(xml_find_all(page,"//table[2]//tr/td[2]"))

test3<-data.frame(quarters[3:40],subs[2:39])
names(test3)[1]<-"Year"
names(test3)[2]<-"Subscribers in Millions"
Netflix_Subs<-test3
Netflix_Subs
View(Netflix_Subs)
Netflix_Subs$`Subscribers in Millions`<-as.numeric(Netflix_Subs$`Subscribers in Millions`)

#USA Unemployment Data
unemployment<-read.csv("american_unemployment_rate.csv")
View(unemployment)

names(unemployment)[1]<-"Year"

#Merged Netflix and Unemployment Data
unemployment_netflix <- merge(Netflix_Subs, unemployment, by.x = "Year")

#Separating Year and Quarter
#install.packages("tidyr")
library(tidyr)
unemployment_netflix<-extract(unemployment_netflix, Year, c("Quarter","Year"), "([^ ]+) (.*)")
View(unemployment_netflix)

unemployment_netflix <- unemployment_netflix[,c(2, 1, 3:11)]
View(unemployment_netflix)
unemployment_netflix <- unemployment_netflix[order(unemployment_netflix$Year),]
View(unemployment_netflix)
unemployment_netflix$Year<-as.numeric(unemployment_netflix$Year)

#USA Netflix Data
More_data<-xml_find_all(page, "//table[5]")
More_rows<-xml_find_all(page,"//table[5]//tr/td[1]")
More_quarters<-xml_text(xml_find_all(page,"//table[5]//tr/td[1]"))
USA_Data<-xml_text(xml_find_all(page,"//table[5]//tr/td[2]"))
Netflix_USA<-data.frame(More_quarters[3:31], USA_Data[2:30])
names(Netflix_USA)[1]<-"Date"
names(Netflix_USA)[2]<-"Subscribers in Millions"

#International Netflix Data
Int_data<-xml_find_all(page, "//table[5]")
Int_rows<-xml_find_all(page,"//table[5]//tr/td[1]")
Int_quarters<-xml_text(xml_find_all(page,"//table[5]//tr/td[1]"))
Int_Data<-xml_text(xml_find_all(page,"//table[5]//tr/td[3]"))
Netflix_Int<-data.frame(Int_quarters[3:31], Int_Data[2:30])
names(Netflix_Int)[1]<-"Date"
names(Netflix_Int)[2]<-"Subscribers in Millions"

#Merged USA and International Netflix Data
More_Netflix<-merge(Netflix_USA,Netflix_Int, by = "Date")
names(More_Netflix)[2]<-"USA"
names(More_Netflix)[3]<-"International"
View(More_Netflix)
library(tidyr)
More_Netflix<-extract(More_Netflix, Date, c("Quarter","Year"), "([^ ]+) (.*)")
More_Netflix <- More_Netflix[order(More_Netflix$Year),]

More_Netflix$Year<-as.numeric(More_Netflix$Year)
More_Netflix$USA<-as.numeric(More_Netflix$USA)

#BarPlot
quarterly_subscribers<-table(unemployment_netflix$`Subscribers in Millions`,unemployment_netflix$Quarter)
quarter_subscribers<-subset(unemployment_netflix, Quarter=="Q1" | Quarter=="Q2" | Quarter=="Q3" | Quarter=="Q4",select = c("Quarter","Subscribers in Millions"))
View(quarter_subscribers)
quarter_subscribers<-subset(unemployment_netflix, Quarter=="Q1" | Quarter=="Q2" | Quarter=="Q3" | Quarter=="Q4", 
                            select = c("Quarter","Subscribers in Millions"))
View(quarter_subscribers)

total_subscribers<-aggregate(quarter_subscribers$`Subscribers in Millions`,by=list(quarter_subscribers$Quarter),FUN=sum,na.rm=TRUE)
total_subscribers

quarter_totals<-c(787.12,819.84,874.87,928.45)
barplot(quarter_totals, ylim = c(0,1000), main = "Total Subscribers Based on Quarter From 2011-2018
        ", xlab = "Quarters", ylab = "Subscribers in Millions", names.arg =c( "Q1", "Q2", "Q3","Q4"),col = c("red", "blue", "orange", "green"))

#Netflix USA Line Graph
years<-aggregate(More_Netflix$USA, by=list(More_Netflix$Year), FUN=sum)
years

plot(years, type="l", main="USA Netflix Subscribers 2011-2018", xlab="Year", ylab="Subscribers in Millions", col="red")

#CorrelationV
Growth<- matrix(c(0,0.465,13.102,5.239,6.923,10.440,12.780,4.059,6.680,8.997,11.368,4.009,5.543,7.562,
                  9.435, 5.183, 5.278, 7.301, 9.698, 2.818, 4.230, 6.976, 5.915,
                  4.960,5.028,6.364,7.465,4.584,4.881,6.778,6.893,1.814,4.467,5.533,9.438,5.518,1.140,4.361))
growth_unemploy_cor<-cbind(unemployment_netflix, Growth)
View(growth_unemploy_cor)
correlation <- lm(growth_unemploy_cor$Growth~growth_unemploy_cor$Total)
summary(correlation)

## Unemployment Summary Statistics ##
library(dplyr)
monthly_unemployment <- read.csv("2001_2021_unemployment.csv")
names(monthly_unemployment)[1]<-"Year"
## Data frames
mean_unemployment<-aggregate(Unemployment~Year, data=monthly_unemployment, FUN=mean,
                             na.rm=TRUE)
max_unemployment<-aggregate(Unemployment~Year, data=monthly_unemployment, FUN=max,
                            na.rm=TRUE)
min_unemployment <- aggregate(Unemployment~Year, data=monthly_unemployment, FUN=min,
                              na.rm=TRUE)
## Merge
unemployment_range <- merge(min_unemployment, max_unemployment, by = "Year")
unemployment_summary <- merge(unemployment_range, mean_unemployment, by.x = "Year", by.y = "Year")
## Rename columns
names(unemployment_summary)[2]<-"Min"
names(unemployment_summary)[3]<-"Max"
names(unemployment_summary)[4]<-"Mean"
unemployment_summary

#Histogram Netflix Usage
hist(as.numeric(More_Netflix$International),
     
     main="International Netflix Usage 2011-2018",
     
     xlab = "Usage %",
     
     border = "red",
     
     col = "black")

hist(as.numeric(More_Netflix$USA),
     
     main="U.S. Netflix Usage 2011-2018",
     
     xlab = "Usage %",
     
     border = "red",
     
     col = "black")

#Unemployment Line Graph
unemployment_2001<-read.csv("2001_2021_unemployment.csv")
names(unemployment_2001)[1]<-"Year"
plot(unemployment_2001$Year,unemployment_2001$Unemployment, type = "l",col="red",
     xlab = "Year",ylab = "Unemployment Rate (%)", main = "Unemployment 2001-2021")

#Export File, this is our final merged table
#write.csv(unemployment_netflix, "unemployment_netflix.csv")