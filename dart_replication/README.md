#This folder includes a R script and R data files for DART replication for manuscript CMPS-19-0088.R1 entitled "Diplomatic Documents Data for International Relations: The Computational and Historical Resources on Nations and Organizations for the Social Sciences Database".

1. [File "dart_replication.R"](dart_replication.R) is the R script. This code was written and ran on/with:
- R version 3.5.2 (2018-12-20)
- Platform: x86_64-apple-darwin15.6.0 (64-bit)
- Running under: macOS High Sierra 10.13.6

Before running the script install the following packages:
- tidyverse_1.2.1
- foreign_0.8-71  
- ggplot2_3.2.1   
- gridExtra_2.3   
- lubridate_1.7.4 

Other (automatically) attached R (base) packages include:
- stats     
- graphics  
- grDevices 
- utils     
- datasets  
- methods   
- base     
- forcats_0.4.0  
- stringr_1.4.0   
- dplyr_0.8.4     
- purrr_0.3.3     
- readr_1.3.1    
- tibble_2.1.3    


2. [File "cfpf_month.RData"](cfpf_month.RData) is the document-month-level dataset for the number of documents (cables) in the Central Foreign Policy Files corpus by month-year and includes the following variables:
- `doc_id`: document number for each document          
- `month`: month-year in which each document was sent  (note that the day is set to the first day of each month for graphs)    
- `classification`: classification level of each document

3. [File "frus_year.RData"](frus_year.RData) is the document-year-level dataset for the number of documents in the Foreign Relations of the U.S. corpus by year and includes the following variables:
 - `doc_id`: document number for each document
 - `year`: year in which each document was sent (note that the day and month is set to the first day of January of each year for graphs)
 - `classification`: classification level of each document

4. [File "non_us_country_tag_traffic_year.RData"](non_us_country_tag_traffic_year.RData) is the dataset for the number of documents (cables) that each non-U.S. contemporary country-year received from the U.S. included in the Central Foreign Policy Files. It includes the following variables:
- `cow_ccode`: Correlates of War (COW) country code for each receiving country
- `cow_statename`: COW state name for each receiving country
- `cow_stateabb`: COW state abbreviation of the country to which the document was sent
- `year`: year in which the documents were sent
- `n_c_y`: number of documents sent to each country-year
	
5. [File "non_us_country_tag_traffic.RData"](non_us_country_tag_traffic.RData) is the dataset for the number of documents (cables) that each non-U.S. contemporary country received from the U.S. included in the Central Foreign Policy Files. It includes the following variables:
- `cow_ccode`: Correlates of War (COW) country code for each receiving country
- `cow_statename`: COW state name for each receiving country
- `cow_stateabb`: COW state abbreviation of the country to which the document was sent
- `year`: year in which the documents were sent
- `n_c`: number of documents sent to each country



