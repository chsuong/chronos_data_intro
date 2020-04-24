#R Script for DART Replication for Manuscript CMPS-19-0088.R1
#"Diplomatic Documents Data for International Relations: The Computational and Historical Resources on Nations and Organizations for the Social Sciences Database"


#Set-up

##Load libraries and set the working directory
rm(list = ls()) # clear objects in memory
library(tidyverse)
library(foreign)
library(ggplot2)
library(gridExtra)

##Set the working directory and the driver
setwd("~/chronos_data_intro/dart_replication")


#Section 4.1. Data Overview

##TABLE 1: Overview of the Corpora (NEEDS RAYMOND'S INPUT)







#Section 4.4. Central Foreign Policy File Collection

##FIGURE 3: Bar Graph of Number of Cables by Month

load("cfpf_month.RData")
ggplot(cfpf_month, aes(month)) + 
  geom_bar(aes(fill=classification)) +
  scale_x_date(breaks=scales::pretty_breaks(10)) +
  scale_y_continuous(breaks=scales::pretty_breaks(10)) +
  labs(y = "Number of Documents",
       x = "Month") + 
  scale_fill_grey(start=0.8, end=0.2) +
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)) + 
  labs(fill = "Classification")  


#4.5. Foreign Relations of the United States Collection

##FIGURE 5: Number of FRUS Documents by Year (NEEDS RAYMOND'S INPUT)

load("frus_year.RData")
frus_year<-subset(frus_year,year(year)>=1860)
png("frus_n_year_class.png", width = 6, height = 6, units = 'in', res = 300)
ggplot(frus_year, aes(year)) + 
  geom_bar(aes(fill=classification)) +
  scale_x_date(breaks=scales::pretty_breaks(10)) +
  scale_y_continuous(breaks=scales::pretty_breaks(10)) +
  labs(y = "Number of Documents",
       x = "Year") + 
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        legend.position = c(0.1, 0.9), 
        legend.justification = c(0.1, 0.9))   +
  scale_fill_grey(start=0.8, end=0.2) + 
  labs(fill = "Classification")  
dev.off()

#5.1. Descriptive Statistics of Country TAG Traffic

##FIGURE 7: Country TAG Traffic at Country-Year and Country Levels

load("non_us_country_tag_traffic_year.RData")
p1<-ggplot(non_us_country_tag_traffic_year, aes(n_c_y)) +
  geom_histogram(bins = 300) +
  theme_bw() +
  labs(title = "By Country-Year",
       y = "Count",
       x = "Country TAG Traffic") +
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11))

load("non_us_country_tag_traffic.RData")
p2<-ggplot(non_us_country_tag_traffic, aes(n_c)) +
  geom_histogram(bins = 300) +
  theme_bw() +
  labs(title = "By Country",
       y = "Count",
       x = "Country TAG Traffic") +
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11)) +
  xlim(0, 151000)
grid.arrange(p1, p2)


#5.2. Validation and Extensions: Using Other Collections

##FIGURE 8: Political and Economic Cables to OAPEC Nations as a Proportion of All Such Cables Sent (NEEDS RAYMOND'S INPUT)

load("oapec.RData")


y=.12
png("oapec_perc.png", width = 6, height = 6, units = 'in', res = 300)
par(mar = c(2,4,2,2))
ts.plot(weekly.data$oapec.perc, gpars=list(xaxt="n"),xlab="", ylab="OAPEC cables/all cables (Pol/Econ)")
abline(v=21, col="black", lty=3)
text (x=4, y=y, "Oil embargo\nbegins", cex=.5)
abline(v=42, col="black", lty=3)
text (x=60, y=y, "Oil embargo\nends", cex=.5)
xyear<-c(1, 53, 105, 157, 209, 261,313)
#title("Pol/Econ cables to OAPEC as % of All Pol/Econ cables", cex=.5)
axis(1, at=xyear,labels=c("6/1/73", "5/31/74", "5/30/75", "5/28/76", "5/27/77", "5/26/78", "5/25/79"), cex.axis=.7)
dev.off()





