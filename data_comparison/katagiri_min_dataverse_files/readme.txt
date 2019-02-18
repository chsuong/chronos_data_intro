To replicate the results as smoothly as possible, do the following:

 - Save the R files (PAR_Code.R, plotsAndAnalysis.R, predict.R) to the main folder.
 - Place all .csv files in a new subfolder named "data"
 - Create a new subfolder named "figures" and leave it empty. Figures from the R code will be saved here. 
 
 - Run predict.R to replicate segment predictions and generate weekly-level data. Be sure to set the working directory to the "data" subfolder.
 - Run plotsAndAnalysis.R to produce figures from the article and to replicate the time series results. (This file will call for PAR_Code.R.) Be sure to set the working directory to the "data" subfolder. Note that the replication materials include the files created by predict.R, so it is not necessary to run predict.R in order to replicate the figures or time series results. See below.

 