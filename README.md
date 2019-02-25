I have looked at some metadata for documents in the database 'declassification_cables' in a very rudimentary fashion. I'm in the process of expanding and looking at other databases as well.

I created a html/rmarkdown file to explore the databases and to make Word-compatible tables. I will migrate to Python for analyzing the text. 

- A summary/commentary of data introduction papers: https://docs.google.com/document/d/116FIdxxrGXxwO-AweaXaL6Kng_tmcYR8KOgpqBzBOOU/edit?usp=sharing

- Outline/draft: https://docs.google.com/document/d/1Ve1iRiVqhL-jpKFY5x0dZGl_irYQpUeW_FmIMxlinK8/edit?usp=sharing

- Notes on diplomacy: https://docs.google.com/document/d/10stYnmi4122yy8v10FoHj-9YBbFBWtHEM-3h3e_x-IU/edit?usp=sharing

- Notes on US foreign policy events in the 1970s: https://docs.google.com/document/d/1J6abyWf8cuLY4-dySXti-KskrCneEtPkP8iHrrhSxQQ/edit?usp=sharing

##Sources for external datasets in the folder "external_data":

* COW country codes (cow): http://www.correlatesofwar.org/data-sets/cow-country-codes/cow-country-codes/at_download/file
* COW diplomatic representation (us_dip_rep_cow): http://www.correlatesofwar.org/data-sets/diplomatic-exchange
* U.S. diplomatic representation (us_dip_rep_moyer; COW-compatible version): https://www.dropbox.com/sh/2wnklx04vblnmi1/AABmMxbxvja_JVStsxKD4F2Qa?dl=0
** Errors:

* U.S. diplomatic visits (us_dip_vis): https://academic.oup.com/isq/article/60/1/107/2358022#supplementary-data
** Errors: the variables for visits bi_SOS and bi_PRE (and the dataset as a whole) include countries that never existed during the period, hence should not appear in the dataset. The variables have the value of 0 for Croatia, Macedonia (cowid 343), Bosnia and Herzegovina (cowid 346), Kosovo (cowid 347) for the 1970s. However, the countries did not exist in the 1970s. Even when they are dropped in analyses using control variables with missing values, they should not be coded as 0.   

* U.S. diplomatic appointments (us_dip_app): https://static-content.springer.com/esm/art%3A10.1007%2Fs11558-017-9277-0/MediaObjects/11558_2017_9277_MOESM1_ESM.zip
* UN voting (un_vote): https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
* Actor-level International Crisis Behavior (icb; Version 12): https://sites.duke.edu/icbdata/data-collections/
* U.S. diplomatic events (us_dip_evt): https://www.dropbox.com/sh/d93tdqanxugvlvg/AAByV97aMEE1Ydh0mkihigrSa?dl=0

