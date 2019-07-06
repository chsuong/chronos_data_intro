#Load libraries and set the working directory
rm(list = ls()) # clear objects in memory
library(plyr)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(RMySQL) #For connecting to the databse
library(htmlTable) #For creating Word-compatible tables
library(lubridate) #For temporal variables
library(zoo) #For temporal variables
library(foreign)
library(ggplot2)
library(reshape2)
library(countrycode) #For reconciling different country codes across dataset
library(ISOcodes) #A package for ISO country codes
library(stargazer)
library(rowr) #For cbind with fill
library(gridExtra)

#Set-up

##Set the working directory
setwd("/Users/clarahsuong/chronos_data_intro")

## Download the following datasets in the subdirectory "external_data"
### COW country codes (cow): http://www.correlatesofwar.org/data-sets/cow-country-codes/cow-country-codes/at_download/file
### National Material Capabilities (v5.0) (nmc): http://www.correlatesofwar.org/data-sets/national-material-capabilities


#Data Overview

##List the collections
driver = dbDriver("MySQL")
connection = dbConnect(driver,host='history-lab.org', password='XreadF403', user='de_reader')
dbGetQuery(connection, 'show databases;')

##Download the table "docs" for all databases
###Store the tables "docs" for database "declassification_cables" and "declassification_frus" 
###somewhere with no file size restriction.
db_docs <- function(mydb) {
  mydb2 = dbConnect(driver,host='history-lab.org', password='XreadF403', user='de_reader', dbname=mydb)
  docs<-dplyr::tbl(mydb2, 'docs') %>% 
    collect(n = Inf) %>%
    distinct()
  return(docs)
}
cables_docs<-db_docs('declassification_cables')
frus_docs<-db_docs('declassification_frus') 
clinton_docs<-db_docs('declassification_clinton') 
pdb_docs<-db_docs('declassification_pdb')
kissinger_docs<-db_docs('declassification_kissinger')
ddrs_docs<-db_docs('declassification_ddrs')
cabinet_docs<-db_docs('declassification_cabinet')
cpdoc_docs<-db_docs('declassification_cpdoc')


#Section 4.1. Data Overview

##TABLE 1: Overview of the Corpora

###Number of documents and date ranges for each collection
db_doc_no_date <- function(mydb) {
  mydb2<-eval(parse(text=paste(mydb, sep = "")), env=.GlobalEnv)
  mydb2<-mydb2  %>%
    select(id, date) %>%
    collect() %>%
    distinct()
  return(c(nrow(mydb2), range(mydb2$date, na.rm = TRUE)))
}
db_doc_no_date('cables_docs')
db_doc_no_date('frus_docs')
db_doc_no_date('pdb_docs')
db_doc_no_date('kissinger_docs')
db_doc_no_date('clinton_docs') 
db_doc_no_date('ddrs_docs')
db_doc_no_date('cabinet_docs')
db_doc_no_date('cpdoc_docs')

###Number of documents with full text vs. non-full text
sum(!is.na(cables_docs$body))  
sum(!is.na(frus_docs$body))
sum(!is.na(pdb_docs$body))
sum(!is.na(kissinger_docs$body))
sum(!is.na(clinton_docs$body))
sum(is.na(ddrs_docs$body))
sum(!is.na(cabinet_docs$body))
sum(!is.na(cpdoc_docs$body))

sum(sum(!is.na(cables_docs$body)),
    sum(!is.na(frus_docs$body)),
    sum(!is.na(pdb_docs$body)),
    sum(!is.na(kissinger_docs$body)),
    sum(!is.na(clinton_docs$body)),
    sum(is.na(ddrs_docs$body))
    )

sum(sum(!is.na(cables_docs$body)),
    sum(!is.na(frus_docs$body)),
    sum(!is.na(pdb_docs$body)),
    sum(!is.na(kissinger_docs$body)),
    sum(!is.na(clinton_docs$body)),
    sum(is.na(ddrs_docs$body)),
    sum(!is.na(cabinet_docs$body)),
    sum(!is.na(cpdoc_docs$body))
    )

sum(is.na(cables_docs$body))
sum(is.na(frus_docs$body))
sum(is.na(pdb_docs$body))
sum(is.na(kissinger_docs$body))
sum(is.na(clinton_docs$body))
sum(!is.na(ddrs_docs$body))
sum(is.na(cabinet_docs$body))
sum(is.na(cpdoc_docs$body))

sum(sum(is.na(cables_docs$body)),
    sum(is.na(frus_docs$body)),
    sum(is.na(pdb_docs$body)),
    sum(is.na(kissinger_docs$body)),
    sum(is.na(clinton_docs$body)),
    sum(!is.na(ddrs_docs$body))
    )

sum(sum(is.na(cables_docs$body)),
    sum(is.na(frus_docs$body)),
    sum(is.na(pdb_docs$body)),
    sum(is.na(kissinger_docs$body)),
    sum(is.na(clinton_docs$body)),
    sum(!is.na(ddrs_docs$body)),
    sum(is.na(cabinet_docs$body)),
    sum(is.na(cpdoc_docs$body))
    )

#Section 4.4. Central Foreign Policy File Collection

#Appendix A: Details on the CFPF Corpus

##TABLE 2: Number of Cables with Non-Missing Values by Variable
sum(!is.na(cables_docs$collection))
sum(!is.na(cables_docs$id))
sum(!is.na(cables_docs$body))
sum(!is.na(cables_docs$date))
sum(!is.na(cables_docs$classification))
sum(!is.na(cables_docs$subject))
sum(!is.na(cables_docs$from_field))
sum(!is.na(cables_docs$to_field))
sum(!is.na(cables_docs$concepts))
sum(!is.na(cables_docs$office))
sum(!is.na(cables_docs$type))

##TABLE 3: Number of Cables by Year
table_cables_n_year<-
  cables_docs %>%
  mutate(year=lubridate::year(date)) %>%
  group_by(year) %>%
  tally() %>%
  mutate(total_n = sum(n),
         rel.freq = paste0(round(100 * n/total_n, 2), "%")) %>%
  select(year, n, rel.freq) %>%
  adorn_totals("row")

table_cables_n_year

##TABLE 4: Number of Cables by Classification Level
stargazer(classification_doc2[c("classification","n", "rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Number of Documents By Classification Level", 
          digits=1, 
          out="./data_analysis_output/table_cables_n_class.txt",
          covariate.labels=c("Classification","Number of Documents", "Relative Frequency"))

#FRUS Collection
##FIGURE: Number of Documents by Year and Classification
frus_n_date<-
  frus_docs %>%
  dplyr::select(id, date, classification) %>%
  mutate(date=as_date(date), 
         Classification = replace_na(classification, "Missing"),
         year = as_date(cut(date, breaks = "year")),
         Classification =factor(Classification, levels = c("Missing", "Confidential","Secret","Top Secret")))

##Frequency Tables 
###TABLE: Number of Documents with Non-Missing Values by Variable
C1<-c("collection",
      "id",
      "body",
      "date",
      "classification",
      "volume_id",
      "chapt_title",
      "title", 
      "p_from",
      "p_to",
      "source"
)

C2<-c(sum(!is.na(frus_docs$collection)),
      sum(!is.na(frus_docs$id)),
      sum(!is.na(frus_docs$body)),
      sum(!is.na(frus_docs$date)),
      sum(!is.na(frus_docs$classification)),
      sum(!is.na(frus_docs$volume_id)),
      sum(!is.na(frus_docs$chapt_title)),
      sum(!is.na(frus_docs$title)),
      sum(!is.na(frus_docs$p_from)),
      sum(!is.na(frus_docs$p_to)),
      sum(!is.na(frus_docs$source))
)

table_frus_n_na<-cbind(C1, C2)
colnames(table_frus_n_na) <- c("Variable","Number of Documents with Non-Missing Values")

stargazer(table_frus_n_na,
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Number of Documents with Non-Missing Values by Variable", 
          digits=1, 
          out="./data_analysis_output/table_frus_n_na.txt"
)


###TABLE: Number of Documents by Year
table_frus_n_year<-
  frus_docs %>%
  mutate(year=lubridate::year(date)) %>%
  group_by(year) %>%
  tally() %>%
  mutate(total_n = sum(n),
         rel.freq = paste0(round(100 * n/total_n, 2), "%")) %>%
  ungroup() %>% 
  adorn_totals("row")

stargazer(table_frus_n_year[c("year","n", "rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Number of Documents By Year", 
          digits=1, 
          out="./data_analysis_output/table_frus_n_year.txt",
          covariate.labels=c("Year","Number of Documents", "Relative Frequency")
)


###TABLE: Number of Documents by Classification
table_frus_n_class<-
  frus_docs %>%
  mutate(year=lubridate::year(date)) %>%
  group_by(classification) %>%
  tally() %>%
  mutate(total_n = sum(n),
         rel.freq = paste0(round(100 * n/total_n, 2), "%")) %>%
  ungroup() %>% 
  adorn_totals("row")

stargazer(table_frus_n_class[c("classification","n", "rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Number of Documents By Classification Level", 
          digits=1, 
          out="./data_analysis_output/table_frus_n_class.txt",
          covariate.labels=c("Classification","Number of Documents", "Relative Frequency"))

#Country TAG Traffic

##Examine the different country codes across datasets
mydb = dbConnect(driver,host='history-lab.org', password='XreadF403', user='de_reader', dbname='declassification_cables')

#A list of countries according to our database
countries<-
  tbl(mydb, 'countries') %>% 
  collect() 
#This table is incomplete. Note that there is no tag for "South Vietnam" but tag "VM" (id: 557) for "Vietnam" according to this table. The 1976 TAGS manual lists VN (tag_id 1976) for "Viet-Nam (North)"and VS (tag_id 1973) for "Viet-Nam (South)". There is also no tag_id for Serbia, Montenegro, Croatia, Azerbaijan. 
#US is included.
#Note there is no tag_id for the Soviet Union but one for Russia.

#Merge ISO_3166_1 and ISO_3166_3 (ISO country codes for withdrawn countries). Note that this list often omits retired codes. 
iso_3166<-
  tibble::as_tibble(full_join(ISO_3166_1, ISO_3166_3, by = c("Alpha_3","Numeric","Name")))%>%
  mutate(Numeric=as.integer(Numeric)) %>%
  dplyr::select("Alpha_3",
                "Numeric",
                "Name",
                "Official_name",
                "Common_name") 

#Generate a dataframe for all (former and existing) countries according to COW. Note that this includes the US.
all_states<-
  read_csv("./external_data/cow/states2016.csv") %>%
  dplyr::select("stateabb","ccode","statenme") %>%
  #filter(!ccode==2) %>% #Leave out the US
  rename(cow_ccode=ccode,
         cow_stateabb=stateabb, 
         cow_statename=statenme) %>%   
  mutate(cow_stateabb=as.character(cow_stateabb),
         cow_statename=as.character(cow_statename)) %>%
  distinct() #There are duplicates. e.g. countries that existed, disappeared, and then re-appeared. 

#Generate a dataframe for all (former and existing) countries for years 1973-79. Note that this includes the US.
all_states_year<-
  all_states %>%
  rowr::cbind.fill(c(1973:1979),fill = NA) %>%
  rename(year=object) %>%
  expand(year = 1973:1979, nesting(cow_stateabb, 
                                   cow_ccode,                    
                                   cow_statename))

#Generate a dataframe for countries existing during the period of 1973-79. Note that the universe of countries differs by year due to the difference in countries' birth/death years. Note that this also includes the US.
states_70s_year<-
  read_csv("./external_data/cow/system2016.csv") %>%
  dplyr::select("stateabb","ccode","year") %>%
  filter(year>1972 & year<1980) %>%
  rename(cow_ccode=ccode, 
         cow_stateabb=stateabb) %>%
  left_join(all_states, by=c("cow_ccode","cow_stateabb")) #Include COW state names.

states_70s<-
  states_70s_year %>%
  dplyr::select(-year) %>%
  distinct() 

##Create a dataframe linking country codes and tag_ids
#Note that this includes country codes and tag_id for the US.
country_code_tag<-
  tbl(mydb, 'countries') %>% 
  collect() %>%
  mutate(country_id=as.integer(id)) %>%
  dplyr::select(-id) %>%
  mutate(cow_ccode=countrycode(name, 'country.name', 'cown')) %>% #Derive COW country codes from the variable "name" in the table "countries."
  mutate(iso3n=countrycode(name, 'country.name', 'iso3n')) #Derive iso numeric country codes from the variable "name" in the table "countries."

#Check whether the variable "country_id" in the table "countries" is from ISO 3166.
#cow_ccode for Vietnam should be 816, not 817 (error in the package countrycode) and cow_ccode for West Germany (German Federal Republic) should be 260.
#iso3n for South Vietnam should not be 704 (country code of Vietnam) but 714 (error in the R package countrycode). The tag_id for South Vietnam is missing.
#country_code_tag$cowid2<-countrycode(country_code_tag$country_id, 'iso3n', 'cown')

all(country_code_tag$country_id %in% iso_3166$Numeric)
all(iso_3166$Numeric %in% country_code_tag$country_id)
setdiff(country_code_tag$country_id, iso_3166$Numeric)
country_code_tag[country_code_tag$country_id %in% setdiff(country_code_tag$country_id, iso_3166$Numeric),]
#Most of the items with a discrepancy between the database's country_id and iso-3166 numeric seem to be defunct states or non-state entities, except East and West Germany.

#Replace the wrong COW country codes and tag_id
country_code_tag<-
  country_code_tag %>%
  mutate(cow_ccode= replace(cow_ccode, name=="Vietnam", 816)) %>% #Fix cow_ccode for Vietnam 
  mutate(cow_ccode= replace(cow_ccode, name=="West Germany", 260)) %>% #Fix cow_ccode for West Germany (German Federal Republic) 
  mutate(tag_id=replace(tag_id, name=="South Vietnam", 1973)) %>% #Insert tag_id for South Vietnam
  rbind(c("Vietnam",
          0,
          1,
          1976,
          704,
          816,
          704
  )
  ) %>% #Insert the second tag_id value (1976) for Vietnam 
  mutate(cow_ccode=as.integer(cow_ccode),
         tag_id=as.integer(tag_id)) %>%
  inner_join(all_states, by="cow_ccode") %>% #Include state names from the COW list of all states that have ever existed.
  rename(country_name=name) %>% 
  filter(!is.na(cow_ccode) & !is.na(tag_id)) %>% #Drop the observations with missing COW country code and missing tag_id.
  dplyr::select(-iso3n) #Note the 2 tags for Vietnam.

##Tag traffic by country-year and by country

country_tag_doc2

cable_n_country_day

#Yearly tag traffic by state-year, including 0 cables by some countries that did not exist in the 1970s. Note that this does include the cables tagged with US. 
cable_n_all_states_year

#Tag traffic by state, including 0 cables by some countries that did not exist in the 1970s. Note that this does include the cables tagged with US. 
cable_n_all_states

#Yearly tag traffic by state-year, excluding 0 cables by some countries that did not exist in the 1970s. Note that this does include the cables tagged with US. 
cable_n_states_70s_year

#Tag traffic by state, excluding 0 cables by some countries that did not exist in the 1970s. Note that this does include the cables tagged with US. 
cable_n_states_70s

#Note that the total ns for each dataset for differs a bit.

###TABLE: Summary Statistics of Country TAG Traffic by Country-Year (Only Contemporary Non-US Countries)
stargazer(as.data.frame(cable_n_states_70s_year[cable_n_states_70s_year$cow_ccode!=2,])[c("year", "cow_ccode", "n_c_y")], 
          type = "text", 
          title="Summary Statistics of Tag Traffic by Country-Year (Only Contemporary Non-US Countries)", 
          digits=1, 
          out="./data_analysis_output/desc_cable_n_nonus_states_70s_year.txt",
          covariate.labels=c("Year", "COW Codes of Countries", "Country TAG Traffic"))

###TABLE: Summary Statistics of Country TAG Traffic by Country (Only Contemporary Non-US Countries) 
stargazer(as.data.frame(cable_n_states_70s[cable_n_states_70s$cow_ccode!=2,])[c("cow_ccode", "n_c")], 
          type = "text", 
          title="Summary Statistics of Tag Traffic by Country (Only Contemporary Non-US Countries)", 
          digits=1, 
          out="./data_analysis_output/desc_cable_n_nonus_states_70s.txt",
          covariate.labels=c("COW Codes of Countries", "Country TAG Traffic"))

###FIGURE: Country TAG Traffic at Country-Year and Country Levels
options(scipen=10000000)

p1<-ggplot(cable_n_states_70s_year[cable_n_states_70s_year$cow_ccode!=2,], aes(n_c_y)) +
  geom_freqpoly(bins = 300) +
  theme_bw() +
  labs(title = "By Country-Year",
       #subtitle = "Data Plotted by Year",
       y = "Frequency",
       x = "Country TAG Traffic"
  ) + 
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)#, 
        #legend.title=element_blank()#, 
        #legend.position = c(0.1, 0.9), 
        #legend.justification = c(0.1, 0.9)
  )

p2<-ggplot(cable_n_states_70s[cable_n_states_70s$cow_ccode!=2,], aes(n_c)) +
  geom_freqpoly(bins = 300) +
  theme_bw() +
  labs(title = "By Country",
       #subtitle = "Data Plotted by Year",
       y = "Frequency",
       x = "Country TAG Traffic") + 
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)#, 
        #legend.title=element_blank()#, 
        #legend.position = c(0.1, 0.9), 
        #legend.justification = c(0.1, 0.9)
  ) +
  xlim(0, 151000)

#png("./data_analysis_output/cable_n_nonus_states_70s_year_freq.png")
grid.arrange(p1, p2)
#dev.off()

###Percentile for Specific Values
ecdf_fun <- function(x,perc) ecdf(x)(perc)
ecdf_fun(cable_n_states_70s_year[cable_n_states_70s_year$cow_ccode!=2,]$n_c_y,5000)
ecdf_fun(cable_n_states_70s_year[cable_n_states_70s_year$cow_ccode!=2,]$n_c_y,10000)-ecdf_fun(cable_n_states_70s_year[cable_n_states_70s_year$cow_ccode!=2,]$n_c_y,5000)
1-ecdf_fun(cable_n_states_70s_year[cable_n_states_70s_year$cow_ccode!=2,]$n_c_y,10000)

ecdf_fun(cable_n_states_70s[cable_n_states_70s$cow_ccode!=2,]$n_c,25000)
ecdf_fun(cable_n_states_70s[cable_n_states_70s$cow_ccode!=2,]$n_c,75000)-ecdf_fun(cable_n_states_70s[cable_n_states_70s$cow_ccode!=2,]$n_c,25000)
1-ecdf_fun(cable_n_states_70s[cable_n_states_70s$cow_ccode!=2,]$n_c,75000)

###TABLE: Summary Statistics of Country TAG Traffic by Country-Year (Including Former Countries and the US)
stargazer(as.data.frame(cable_n_all_states_year)[c("year", "cow_ccode", "n_c_y")], 
          type = "text", 
          title="Summary Statistics of Country TAG Traffic by Country-Year (Incl. Former Countries and the US)", 
          digits=1, 
          out="./data_analysis_output/desc_cable_n_all_states_year.txt",
          covariate.labels=c("Year", "COW Codes of Countries", "Country TAG Traffic"))

###TABLE: Summary Statistics of Country TAG Traffic by Country (Incl. Former Countries and the US)
stargazer(as.data.frame(cable_n_all_states)[c("cow_ccode", "n_c")], 
          type = "text", 
          title="Summary Statistics of Country TAG Traffic by Country (Incl. Former Countries and the US)", 
          digits=1, 
          out="./data_analysis_output/desc_cable_n_all_states.txt",
          covariate.labels=c("COW Codes of Countries", "Country TAG Traffic"))

###FIGURE: Country TAG Traffic at Country-Year and Country Levels (All Countries)
options(scipen=10000000)

p3<-
  ggplot(cable_n_all_states_year, aes(n_c_y)) +
  geom_freqpoly(bins = 300) +
  theme_bw() +
  labs(title = "By Country-Year",
       #subtitle = "Data Plotted by Year",
       y = "Frequency",
       x = "Country TAG Traffic") + 
  theme(text = element_text(size=15),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)#, 
        #legend.title=element_blank()#, 
        #legend.position = c(0.1, 0.9), 
        #legend.justification = c(0.1, 0.9)
  )

p4<-
  ggplot(cable_n_all_states, aes(n_c)) +
  geom_freqpoly(bins = 300) +
  theme_bw() +
  labs(title = "By Country",
       #subtitle = "Data Plotted by Year",
       y = "Frequency",
       x = "Country TAG Traffic") + 
  theme(text = element_text(size=15),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11)#, 
        #legend.title=element_blank()#, 
        #legend.position = c(0.1, 0.9), 
        #legend.justification = c(0.1, 0.9)
  )

#png("./data_analysis_output/cable_n_all_states_year_freq.png")
grid.arrange(p3, p4)
#dev.off()

###TABLE: Country TAG Traffic vs. Cable Traffic
russia_cable_traffic_1<-
  cables_docs %>% 
  filter(str_detect(to_field, "MOSCOW") |
           str_detect(to_field, "LENINGRAD") |  
           str_detect(from_field, "MOSCOW") |  
           str_detect(from_field, "LENINGRAD")) %>% 
  mutate(year=lubridate::year(date)) %>%
  group_by(year) %>%
  tally()

russia_cable_traffic_2<-
  cables_docs %>% 
  filter(str_detect(to_field, "MOSCOW") |
           #str_detect(to_field, "LENINGRAD") |  
           str_detect(from_field, "MOSCOW") #|  
         #str_detect(from_field, "LENINGRAD")
  )%>% 
  mutate(year=lubridate::year(date)) %>%
  group_by(year) %>%
  tally()

russia_cable_traffic_3<-
  cables_docs %>% 
  filter(#str_detect(to_field, "MOSCOW") |
    str_detect(to_field, "LENINGRAD") |  
      #str_detect(from_field, "MOSCOW") |  
      str_detect(from_field, "LENINGRAD")
  )%>% 
  mutate(year=lubridate::year(date)) %>%
  group_by(year) %>%
  tally()

russia_tag<-
  cable_n_states_70s_year %>%
  filter(cow_statename=="Russia")

russia_tag_cable_traffic<-cbind(russia_tag[c("year", "n_c_y")], 
                                #russia_cable_traffic_1["n"],
                                russia_cable_traffic_2["n"],
                                russia_cable_traffic_3["n"]
)

stargazer(russia_tag_cable_traffic,
          type = "text", 
          #flip = TRUE,
          summary = FALSE,
          rownames = FALSE,
          title="Comparison of Country TAG Traffic and Cable Traffic", 
          digits=1, 
          out="./data_analysis_output/russia_tag_cable_traffic.txt",
          covariate.labels=c("Year", 
                             "Number of Cables Tagged<br>with the USSR", 
                             #"Number of Cables Sent by/to<br>the US Embassy in Moscow<br>and the Consulate General in Leningrad", 
                             "Number of Cables Sent by/to<br>the US Embassy in Moscow",
                             "Number of Cables Sent by/to<br>the US Consulate General in Leningrad")
)


###Country TAG Traffic for Certain Countries

cable_east_germany<-
  country_tag_doc2 %>%
  filter(tag_id==419 & (year==1973 | year==1974)) %>%
  group_by(year) %>%
  tally() 
cable_east_germany

cable_viet<-
  country_tag_doc2 %>%
  filter((tag_id==557 | tag_id==1976) & year>1973) %>%
  group_by(year) %>%
  tally() 
cable_viet

cable_rhodesia<-
  country_tag_doc2 %>%
  filter(tag_id==508 & year==1978) %>%
  group_by(year) %>%
  tally()
cable_rhodesia

###TABLE: Non-US Country-Years with Most Cables
table_tag_state_year_top20<- 
  cable_n_states_70s_year %>%
  filter(cow_ccode!=2) %>%
  mutate(rel.freq = paste0(round(100 * n_c_y/total_n, 2), "%")) %>%
  arrange(desc(n_c_y)) %>%
  top_n(n = 20, wt = n_c_y) %>%
  mutate(cow_statename= replace(cow_statename, cow_statename=="Russia", "Soviet Union")) #Replace "Russia" with "Soviet Union"

stargazer(table_tag_state_year_top20[c("year", "cow_statename", "n_c_y","rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Non-US Country-Years with Highest Tag Traffic", 
          digits=1, 
          out="./data_analysis_output/table_tag_state_year_top20.txt",
          covariate.labels=c("Year","Tagged Country", "Number of Cables", "Relative Frequency"))

###TABLE: Non-US Country-Years Tagged in Fewest Cables
table_tag_state_year_bottom20<- 
  cable_n_states_70s_year %>%
  filter(cow_ccode!=2) %>%
  mutate(rel.freq = paste0(round(100 * n_c_y/total_n, 2), "%")) %>%
  arrange(desc(n_c_y)) %>%
  top_n(n = -20, wt = n_c_y) %>%
  mutate(cow_statename= replace(cow_statename, cow_statename=="Russia", "Soviet Union")) #Replace "Russia" with "Soviet Union"

stargazer(table_tag_state_year_bottom20[c("year", "cow_statename", "n_c_y","rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Non-US Country-Years with Lowest Tag Traffic", 
          out="./data_analysis_output/table_tag_state_year_bottom20.txt",
          covariate.labels=c("Year","Tagged Country", "Number of Cables", "Relative Frequency"))


###TABLE: Countries Most Frequently Tagged in Cables
table_tag_state_top20<- 
  cable_n_states_70s %>%
  filter(cow_ccode!=2) %>%
  #group_by(cow_ccode, cow_stateabb, cow_statename) %>% 
  #summarise(n_c = sum(n_c)) %>%
  #ungroup %>%
  mutate(rel.freq = paste0(round(100 * n_c/total_n, 2), "%")) %>%
  arrange(desc(n_c)) %>%
  top_n(n = 20, wt = n_c) %>%
  mutate(cow_statename= replace(cow_statename, cow_statename=="Russia", "Soviet Union")) #Replace "Russia" with "Soviet Union"

stargazer(table_tag_state_top20[c("cow_statename", "n_c","rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Non-US Countries Most Frequently Tagged in Cables", 
          out="./data_analysis_output/table_tag_state_top20.txt",
          covariate.labels=c("Country", "Number of Cables", "Relative Frequency"))

###TABLE: Non-U.S. Countries Least Frequently Tagged in Cables
table_tag_state_bottom20<- 
  cable_n_states_70s %>%
  filter(cow_ccode!=2) %>%
  mutate(rel.freq = paste0(round(100 * n_c/total_n, 0), "%")) %>%
  arrange(desc(n_c)) %>%
  top_n(n = -20, wt = n_c) %>%
  mutate(cow_statename= replace(cow_statename, cow_statename=="Russia", "Soviet Union")) #Replace "Russia" with "Soviet Union"

stargazer(table_tag_state_bottom20[c("cow_statename", "n_c","rel.freq")],
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Non-US Countries Least Frequently Tagged in Cables", 
          digits=1, 
          out="./data_analysis_output/table_tag_state_bottom20.txt",
          covariate.labels=c("Country", "Number of Cables", "Relative Frequency"))

###FIGURE: Country TAG Traffic of Key Non-US Countries by Month
coi_5_list<-table_tag_state_top20$cow_ccode[1:5]

coi_5<-
  country_tag_doc2  %>%
  filter(cow_ccode %in% coi_5_list) %>%
  mutate(cow_statename= replace(cow_statename, cow_statename=="Russia", "Soviet Union"),
         cow_statename= replace(cow_statename, cow_statename=="German Democratic Republic", "East Germany"), 
         #cow_statename2=ifelse(cow_ccode %in% coi_5_list, cow_statename, "Other"),
         date=as_date(date),          
         month = as_date(cut(date, breaks = "month")))

#png("./data_analysis_output/tag_key5.png", width = 600, height = 450)
ggplot(coi_5, aes(month)) + 
  geom_bar(aes(fill=cow_statename)) +
  scale_x_date(breaks=scales::pretty_breaks(10)) +
  scale_y_continuous(breaks=scales::pretty_breaks(10)) +
  labs(y = "Country Tag Traffic Volume",
       x = "Month") + 
  scale_fill_grey() +
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.x = element_text(size=11), 
        axis.text.y = element_text(size=11), 
        legend.title=element_blank()#, 
        #legend.position = c(0.95, 0.05), 
        #legend.justification = c(0.95, 0.05)
  )
#dev.off()


###TABLE: Country TAG Traffic vs. Total Population
nmc_c_y<-
  read_csv("./external_data/NMC_5_0/NMC_5_0.csv") %>%
  dplyr::select("stateabb", "ccode", "year","tpop") 

pop_c <- 
  read_csv("./external_data/NMC_5_0/NMC_5_0.csv") %>%
  dplyr::select("year","ccode", "tpop") %>%
  filter(1972<year & year<1980 & ccode!=2) %>%
  left_join(states_70s_year, by = c("year"="year","ccode" = "cow_ccode")) %>%
  mutate(tpop=1000*tpop) %>%
  group_by(ccode,cow_statename) %>%
  summarise(mean_tpop=mean(tpop, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(mean_tpop)) %>%
  mutate(mean_tpop_rank=row_number(),
         cow_statename= replace(cow_statename, cow_statename=="Russia", "Soviet Union")) #Replace "Russia" with "Soviet Union"

table_tag_state_top20<-
  table_tag_state_top20 %>%
  mutate(tag_rank=row_number())   

table_tag_state_bottom20<-
  table_tag_state_bottom20 %>%
  mutate(tag_rank=row_number())   

table_tag_pop_state_top20_comp<-
  table_tag_state_top20 %>%
  left_join(pop_c, by="cow_statename") %>%
  dplyr::select("cow_statename","tag_rank","mean_tpop_rank")

stargazer(table_tag_pop_state_top20_comp,
          summary = FALSE,
          rownames = FALSE,
          type = "text", 
          title="Country TAG Traffic vs. Population", 
          out="./data_analysis_output/table_tag_pop_state_top20_comp.txt",
          covariate.labels=c("Top 20 Countries in Country TAG Traffic", "Rank in Country TAG Traffic", "Rank in Mean Population"))


