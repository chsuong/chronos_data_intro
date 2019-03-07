I have looked at some metadata for documents in the database 'declassification_cables' in a very rudimentary fashion. I'm in the process of expanding and looking at other databases as well.

I created a html/rmarkdown file to explore the databases and to make Word-compatible tables. I will migrate to Python for analyzing the text. 

- The original proposal: https://docs.google.com/document/d/1xgIa9FoH3dLcBQmw7xI5gBJ1wh55Bg_Vdtayo8Lzq78/edit#

- A summary/commentary of data introduction papers: https://docs.google.com/document/d/116FIdxxrGXxwO-AweaXaL6Kng_tmcYR8KOgpqBzBOOU/edit?usp=sharing

- Outline: https://docs.google.com/document/d/1gajzoj5UhNc_McXDDJlpFQ5s8vqfFsLjNILBsufzmGs/edit?usp=sharing

- Draft: https://docs.google.com/document/d/1Ve1iRiVqhL-jpKFY5x0dZGl_irYQpUeW_FmIMxlinK8/edit?usp=sharing

- Notes on diplomacy: https://docs.google.com/document/d/10stYnmi4122yy8v10FoHj-9YBbFBWtHEM-3h3e_x-IU/edit?usp=sharing

- Notes on US foreign policy events in the 1970s: https://docs.google.com/document/d/1J6abyWf8cuLY4-dySXti-KskrCneEtPkP8iHrrhSxQQ/edit?usp=sharing

##Sources for external datasets in the folder "external_data":

* COW country codes (cow): http://www.correlatesofwar.org/data-sets/cow-country-codes/cow-country-codes/at_download/file
* U.S. diplomatic representation (us_dip_rep_moyer; COW-compatible version): https://www.dropbox.com/sh/2wnklx04vblnmi1/AABmMxbxvja_JVStsxKD4F2Qa?dl=0
** Errors: The dataset by Arias and Smith 2018 seems more accurate about US diplomatic representation. This dataset misses the case of U.S. embassy in the Bahamas in 1973-75 wherease the latter includes it. The codebook is outdated and omits important information. It seems like the value 0 for the variable "Focus New" represents unreciprocated cases. 

* U.S. diplomatic visits (us_dip_vis): https://academic.oup.com/isq/article/60/1/107/2358022#supplementary-data
** Errors: The variables for visits bi_SOS and bi_PRE (and the dataset as a whole) include countries that never existed during the period, hence should not appear in the dataset. The variables have the value of 0 for Croatia, Macedonia (cowid 343), Bosnia and Herzegovina (cowid 346), Kosovo (cowid 347) for the 1970s. However, the countries did not exist in the 1970s. Even when they are dropped in analyses using control variables with missing values, they should not be coded as 0.   

* U.S. diplomatic appointments (us_dip_rep_app): https://static-content.springer.com/esm/art%3A10.1007%2Fs11558-017-9277-0/MediaObjects/11558_2017_9277_MOESM1_ESM.zip
* U.S. diplomatic events (us_dip_evt): https://www.dropbox.com/sh/d93tdqanxugvlvg/AAByV97aMEE1Ydh0mkihigrSa?dl=0
** Errors: Is VMN: a typo for VNM (Vietnam)? 


