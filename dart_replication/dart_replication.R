# R Script for DART Replication for Manuscript CMPS-19-0088.R1
#"Diplomatic Documents Data for International Relations: The Freedom of Information Archive (FOIArchive) Database"
# Contact: clara.suong@duke.edu

# This code reproduces most figures in the manuscript, 
# namely, Figures 3, 5, 7, 8 in the manuscript, 
# which are also saved in the subdirectory ~/chronos_data_intro/dart_replication/figures

# This code was written and ran on/with:
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.6
## Installed R packages: 
### tidyverse_1.2.1
### foreign_0.8-71  
### ggplot2_3.2.1   
### gridExtra_2.3   
### lubridate_1.7.4 
## Other attached R (base) packages:
### stats     
### graphics  
### grDevices 
### utils     
### datasets  
### methods   
### base     
### forcats_0.4.0  
### stringr_1.4.0   
### dplyr_0.8.4     
### purrr_0.3.3     
### readr_1.3.1    
### tibble_2.1.3    


#Set-up

##Load libraries and set the working directory
rm(list = ls()) # clear objects in memory
library(tidyverse)
library(foreign)
library(ggplot2)
library(gridExtra)
library(lubridate)

##Set the working directory and the driver
setwd("~/chronos_data_intro/dart_replication")


#Section 4.4. Central Foreign Policy File Collection

##FIGURE 3: Bar Graph of Number of Cables by Month

load("cfpf_month.RData")
#png("../figures/figure3_cfpf_n_month_class.png", width = 600, height = 450)
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
#dev.off()


#4.5. Foreign Relations of the United States Collection

##FIGURE 5: Number of FRUS Documents by Year

load("frus_year.RData")
frus_year<-subset(frus_year,year(year)>=1860)

#png("../figures/figure5_frus_n_year_class.png",width = 800, height = 800)
ggplot(frus_year, aes(year)) + 
  geom_bar(aes(fill=classification)) +
  scale_x_date(breaks=scales::pretty_breaks(10)) +
  scale_y_continuous(breaks=scales::pretty_breaks(10)) +
  labs(y = "Number of Documents",
       x = "Year") + 
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=22), 
        axis.text.y = element_text(size=22), 
        legend.position = c(0.1, 0.9), 
        legend.justification = c(0.1, 0.9))   +
  scale_fill_grey(start=0.8, end=0.2) + 
  labs(fill = "Classification")  
#dev.off()


#5.1. Descriptive Statistics of Country TAG Traffic

##FIGURE 7: Country TAG Traffic at Country-Year and Country Levels

load("non_us_country_tag_traffic_year.RData")
p1<-ggplot(non_us_country_tag_traffic_year, aes(n_c_y)) +
  geom_histogram(bins = 300) +
  theme_bw() +
  labs(title = "By Country-Year",
       y = "Count",
       x = "Country TAG Traffic") +
  theme(text = element_text(size=22),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22))

load("non_us_country_tag_traffic.RData")
p2<-ggplot(non_us_country_tag_traffic, aes(n_c)) +
  geom_histogram(bins = 300) +
  theme_bw() +
  labs(title = "By Country",
       y = "Count",
       x = "Country TAG Traffic") +
  theme(text = element_text(size=22),
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size=22)) +
  xlim(0, 151000)

#png("../figures/figure7_country_tag_traffic.png",width = 800, height = 800)
grid.arrange(p1, p2)
#dev.off()

#5.2. Validation and Extensions: Using Other Collections

##FIGURE 8: Political and Economic Cables to OAPEC Nations as a Proportion of All Such Cables Sent 

load("oapec.RData")


y=.12
#png("../figures/figure8_oapec_perc.png", width = 6, height = 6, units = 'in', res = 300)
par(mar = c(2,4,2,2))
ts.plot(oapec.data$oapec.perc, gpars=list(xaxt="n"),xlab="", ylab="OAPEC cables/all cables (Pol/Econ)")
abline(v=21, col="black", lty=3)
text (x=4, y=y, "Oil embargo\nbegins", cex=.5)
abline(v=42, col="black", lty=3)
text (x=60, y=y, "Oil embargo\nends", cex=.5)
xyear<-c(1, 53, 105, 157, 209, 261,313)
#title("Pol/Econ cables to OAPEC as % of All Pol/Econ cables", cex=.5)
axis(1, at=xyear,labels=c("6/1/73", "5/31/74", "5/30/75", "5/28/76", "5/27/77", "5/26/78", "5/25/79"), cex.axis=.7)
#dev.off()


