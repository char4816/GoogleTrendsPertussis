# 1/28/19
# setwd("C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts")
# Run model exploring how sociodemographics predict differences in RMSEs of forecastings

library("lme4")
library("orcutt")
library("pbapply")
library("loo")
library('car')

csvData = read.csv(paste("C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/RMSE_vs_variables_model_input.csv",sep=''), header = TRUE)
RMSEdata <- data.frame(csvData)
colnames(RMSEdata) <- c('Region',	'R2',	'RMSE', 'ACEP',	'AGE',	'POVERTY',	'INTERNET',	'EDUCATION',	'URBAN',	'VACCINATED',	'REPUBLICAN',	'JOB', 'POPULATION')

for(column in colnames(RMSEdata)[4:length(colnames(RMSEdata))]){
  # normalize the data
  # RMSEdata[[column]] <- (RMSEdata[[column]]-min(RMSEdata[[column]]))/(max(RMSEdata[[column]])-min(RMSEdata[[column]]))
  # standardize the data
  RMSEdata[[column]] <- (RMSEdata[[column]]-mean(RMSEdata[[column]]))/sd(RMSEdata[[column]])
}
RMSEdata <- RMSEdata[2:nrow(RMSEdata),] #exclude US overall from the analysis

topAICModel.lm <- lm(RMSE ~	ACEP + AGE + POVERTY + INTERNET + EDUCATION + URBAN + VACCINATED + REPUBLICAN + JOB + POPULATION, data=RMSEdata)
print(summary(topAICModel.lm))
# plot(topAICModel.lm)
# vif(topAICModel.lm)