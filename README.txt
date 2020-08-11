2/10/19 -- README for Google Trends modeling of pertussis
Scripts were written by Christopher Arehart (christopher.h.arehart@colorado.edu) to perform the analysis for:

Arehart, C.H., David, M.Z. & Dukic, V. Tracking U.S. Pertussis Incidence: Correlation of Public Health Surveillance and Google Search Data Varies by State. Sci Rep 9, 19801 (2019). https://doi.org/10.1038/s41598-019-56385-z

https://www.nature.com/articles/s41598-019-56385-z

These scripts were written for a specific google trends epidemiology analysis and are not yet tailored for widespread use
However, there are some parts of this analysis that might prove useful to others doing similar research.
The public availability of these scripts hopefully improves transparency and reproducibility of results.

Follow these scripts in the following order:

###################################################################################
###### The following one python script is for downloading Google Trends data ######
###################################################################################

GT_api_gather.py -- this python script extracts the sepcified google trends data from the google trends API.  
To use this script you will need to put in your own API address.

#########################################################################
###### The following six R scripts generate the 6 modeling methods ######
#########################################################################

estimates_topAIC_models.R -- this R script constructs the 52 regions' lowest AIC models.
This script does not account for autocorrelation and outputs the model estimates in estimates_topAIC_models.csv

estimates_topAIC_models_AR_corrected.R -- this R script does the same as estimates_topAIC_models.R however
it uses the Cochrane Orcutt method to correct for autocorrelation. 
Model estimates are in estimates_topAIC_models_AR_corrected.csv

estimates_all_models_averaged.R -- this R script constructs all possible combinations of models for each 
geographic region.  The posterior probabilities are computed and then model averaging is used to generate
the prediction output in estimates_all_models_averaged.csv

estimates_all_models_averaged_AR_corrected.R -- this R script does the same as 
estimates_all_models_averaged.R however it uses the Cochrane Orcutt method to correct 
for autocorrelation. Model estimates are in estimates_all_models_averaged_AR_corrected.csv

estimates_top_models_averaged.R -- this R script is similar to estimates_all_models_averaged.R with the
additional method of selecting for the top (probable) models rather than all possible models for computing
the posterior probabilites and the model averages.  Model estimates are in estimates_all_models_averaged.csv

estimates_top_models_averaged_AR_corrected.R -- this R script does the same as estimates_top_models_averaged.R
however it uses the Cochrane Orcutt method to correct for autocorrelation.  Model estimates are in
estimates_top_models_averaged_AR_corrected.csv

##############################################################################################
###### The following three R scripts generate plots from the outputs of the previous six #####
##############################################################################################

plots_all_models_estimates_2004-2011.R -- this R script generates a plot for each region over the timespan of
2004-2011 for all 6 modeling methods above. Plots are output in plots_all_models_estimates_2004-2011.pdf

plots_all_models_estimates_2011_forecast.R -- this R script generates a plot for each region's 6 month 
forecasting in 2011 for all 6 modeling methods above.  Plots are output in 
plots_all_models_estimates_2011_forecast.pdf

plots_topAIC_models_estimates_2004-2011.R -- this R script is almost identical to 
plots_all_models_estimates_2004-2011.R however it only plots the top AIC model for each region. Plots are
output in plots_topAIC_models_estimates_2004-2011.pdf

###############################################################################################################
###### The following one R script generates the exploratory model for RMSE vs sociodemographic variables ######
###############################################################################################################

RMSE_vs_variables_model.R -- this R script loads in data from RMSE_vs_variables_model_input.csv and generates 
the exploratory model.

####################################################################################################
###### The following xlsx file contains model summary results for RMSE and adjusted R squared ######
####################################################################################################
RMSE_and_R2_Tables.xlsx

#####################################################################################################
###### This R script generates the heatmap of adjusted R squared and RMSE values for the models ######
#####################################################################################################
RMSE_heatmap.R -- this R script reads in data from RMSE_heatmap.csv to make the heatmap plots

########################################################################
###### The following folder contains Project Tycho level one data ######
########################################################################
/PT_level_1_data/ The level one project tycho data required some sorting to get into individual state timeseries
Original data can be found at https://www.tycho.pitt.edu
