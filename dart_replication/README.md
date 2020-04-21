#This folder includes a R script and R data files for DART replication for manuscript CMPS-19-0088.R1 entitled "Diplomatic Documents Data for International Relations: The Computational and Historical Resources on Nations and Organizations for the Social Sciences Database".

* File "dart_replication.R" is the R script.

* File "cfpf_month.RData" is the document-month-level dataset for the number of documents (cables) in the Central Foreign Policy Files corpus by month-year and includes the following variables:
- doc_id: document number for each document          
- month: month-year in which each document was sent  (note that the day is set to the first day of each month for graphs)    
- classification: classification level of each document

* File "frus_year.RData" is the document-year-level dataset for the number of documents in the Foreign Relations of the U.S. corpus by year and includes the following variables:
 - doc_id: document number for each document
 - year: year in which each document was sent (note that the day and month is set to the first day of January of each year for graphs)
 - classification: classification level of each document

* File "non_us_country_tag_traffic_year.RData" is the dataset for the number of documents (cables) that each non-U.S. contemporary country-year received from the U.S. included in the Central Foreign Policy Files
- cow_ccode: Correlates of War (COW) country code for each receiving country
- cow_statename: COW state name for each receiving country
- cow_stateabb: COW state abbreviation of the country to which the document was sent
- year: year in which the documents were sent
- n_c_y: number of documents sent to each country-year
	
* File "non_us_country_tag_traffic.RData"
- cow_ccode: Correlates of War (COW) country code for each receiving country
- cow_statename: COW state name for each receiving country
- cow_stateabb: COW state abbreviation of the country to which the document was sent
- year: year in which the documents were sent
- n_c: number of documents sent to each country



