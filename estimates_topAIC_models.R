# 1/28/19
# setwd("C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts")

library("lme4")
library("orcutt")
library("pbapply")
library("loo")

# Function to extract the overall ANOVA p-value out of a linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

#List of google search terms and list of geographic regions
googleTrends <- c('bordatella','bordetella','chronic cough','coqueluche','coughing fits','pertusis','pertussis symptoms','pertussis treatment','pertussis','tos ferina','whooping cough adults','whooping cough symptoms','whooping cough treatment','whooping cough','whooping')
statesList = c('US', 'US-AK', 'US-AL', 'US-AR', 'US-AZ', 'US-CA', 'US-CO', 'US-CT', 'US-DC', 'US-DE', 'US-FL', 'US-GA', 'US-HI', 'US-IA', 'US-ID', 'US-IL', 'US-IN', 'US-KS', 'US-KY', 'US-LA', 'US-MA', 'US-MD', 'US-ME', 'US-MI', 'US-MN', 'US-MO', 'US-MS', 'US-MT', 'US-NC', 'US-ND', 'US-NE', 'US-NH', 'US-NJ', 'US-NM', 'US-NV', 'US-NY', 'US-OH', 'US-OK', 'US-OR', 'US-PA', 'US-RI', 'US-SC', 'US-SD', 'US-TN', 'US-TX', 'US-UT', 'US-VA', 'US-VT', 'US-WA', 'US-WI', 'US-WV', 'US-WY')


#mock google data to frame data structures
googlecsvData = read.csv(paste('C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/GT_MAXDATES.csv',sep=''), header = TRUE)
googleDateData <- data.frame(googlecsvData[,1])
colnames(googleDateData) <- c("date")


topModels =c()
posteriorProbability <- numeric(length(statesList))
modelAverage <- data.frame(googlecsvData[,1])
colnames(modelAverage) <- c("date")
pdf("C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-posterior_probability_sequential.pdf",paper="a4r")

for (state in statesList){
  print(state)
  #Read in the Project Tycho incidence data.
  csvData = read.csv(paste("C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/PTlevel1data/Final_Incidence_Data/FinalNATimeSeries/PT_",state,".csv",sep=''), header = FALSE)
  tychoData <- data.frame(csvData[,2])
  colnames(tychoData) <- c('incidence')
  
  #Read in the Google Trends data
  googleData <- data.frame(googleDateData[,1])
  colnames(googleData) <- c("date")
  for (term in googleTrends){
    csvData = read.csv(paste('C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-24-GTdata/',state,'_',term,'.csv',sep=''), header = FALSE)
    if(length(unique(csvData[,2]))!=1)
      googleData[term] <- (csvData[,2]-min(csvData[,2]))/(max(csvData[,2])-min(csvData[,2]))
    else
      googleData[term] <- csvData[,2]
  }
  
  #Add new columns to the dataframe for bordatella/bordetella and pertusis/pertussis
  googleData$bordatellabordetella <- (googleData$bordatella+googleData$bordetella)/(max(googleData$bordatella+googleData$bordetella))
  googleData$pertusispertussis <- (googleData$pertusis+googleData$pertussis)/(max(googleData$pertusis+googleData$pertussis))

  googleDataModelTesting <- googleData
  tychoDataModelTesting <- data.frame(tychoData[1:417,])
  googleData <- googleData[1:391,]
  tychoData <- data.frame(tychoData[1:391,])
  colnames(tychoData) <- c('incidence')
  
  all_predictors <- c('bordatellabordetella', '`chronic cough`', 'coqueluche', '`coughing fits`', 'pertusispertussis', '`pertussis symptoms`', '`pertussis treatment`', '`tos ferina`', '`whooping cough adults`', '`whooping cough symptoms`', '`whooping cough treatment`', '`whooping cough`')
  predictors <- c()
  
  # Select only the columns of googleData that are meaningful (i.e. not all the same value)
  if(length(unique(googleData$bordatellabordetella))!=1)
    predictors <- append(predictors, "bordatellabordetella")
  if(length(unique(googleData$`chronic cough`))!=1)
    predictors <- append(predictors, "`chronic cough`")
  if(length(unique(googleData$coqueluche))!=1)
    predictors <- append(predictors, "coqueluche")
  if(length(unique(googleData$`coughing fits`))!=1)
    predictors <- append(predictors, "`coughing fits`")
  if(length(unique(googleData$pertusispertussis))!=1)
    predictors <- append(predictors, "pertusispertussis")
  if(length(unique(googleData$`pertussis symptoms`))!=1)
    predictors <- append(predictors, "`pertussis symptoms`")
  if(length(unique(googleData$`pertussis treatment`))!=1)
    predictors <- append(predictors, "`pertussis treatment`")
  if(length(unique(googleData$`tos ferina`))!=1)
    predictors <- append(predictors, "`tos ferina`")
  if(length(unique(googleData$`whooping cough adults`))!=1)
    predictors <- append(predictors, "`whooping cough adults`")
  if(length(unique(googleData$`whooping cough symptoms`))!=1)
    predictors <- append(predictors, "`whooping cough symptoms`")
  if(length(unique(googleData$`whooping cough treatment`))!=1)
    predictors <- append(predictors, "`whooping cough treatment`")
  if(length(unique(googleData$`whooping cough`))!=1)
    predictors <- append(predictors, "`whooping cough`")
  
  
  l <- rep(list(c(TRUE,FALSE)), length(predictors))
  reg_mat <- expand.grid(l) #makes true false matrix
  reg_mat <- reg_mat[-(dim(reg_mat)[1]),]
  
  # # let's name the columns
  names(reg_mat) <- paste(predictors, 1:length(predictors), sep="")
  
  all_models_list <- apply(reg_mat, 1, function(x) as.formula(
    paste(c("tychoData$incidence~", predictors[x]), collapse=" + ")) )
  
  # all_models_results <- pblapply(all_models_list,
  #                               function(x) cochrane.orcutt(lm(formula = x, data=googleData, na.action=na.exclude), convergence = 8)) #
  
  all_models_results <- pblapply(all_models_list,
                                 function(x) lm(formula = x, data=googleData, na.action=na.exclude)) #
  
  mod_number <- numeric(2^(length(predictors))-1)
  mod_formula <- character(2^(length(predictors))-1)
  model <- character(2^(length(predictors))-1)
  aic <- numeric(2^(length(predictors))-1)
  bic <- numeric(2^(length(predictors))-1)
  # adj_r_squared <- numeric(2^(length(predictors))-1)
  # r_squared <- numeric(2^(length(predictors))-1)
  # p_value <- numeric(2^(length(predictors))-1)
  posteriorProbabilityVector <- numeric(2^(length(predictors))-1)
  
  for (i in 1:(2^(length(predictors))-1)) {
    mod_name <- toString(i)
    mod_number[i] <- i
    mod_formula[i] <- toString(all_models_list[[mod_name]][3])
    mod_formula[i] <- substring(mod_formula[i], 2, nchar(mod_formula[i]))#remove the harmless + at beginning of string
    # print(summary(all_models_results[[mod_name]])$call)
    model[i] <- toString(summary(all_models_results[[mod_name]])$call)
    aic[i] <- nrow(googleData)*(log(2*pi)+1+log((sum(all_models_results[[mod_name]]$residuals^2)/nrow(googleData))))+((length(all_models_results[[mod_name]]$coefficients)+1)*2)
    bic[i] <- nrow(googleData)*(log(2*pi)+1+log((sum(all_models_results[[mod_name]]$residuals^2)/nrow(googleData))))+((length(all_models_results[[mod_name]]$coefficients)+1)*log(nrow(googleData)))
    # adj_r_squared[i] <- all_models_results[[mod_name]]$adj.r.squared
    # r_squared[i] <- all_models_results[[mod_name]]$r.squared
    # p_value[i] <- lmp(all_models_results[[mod_name]])
  }
  
  result <- data.frame(mod_number, aic, bic, stringsAsFactors=FALSE)
  # We need to only select the model with the smallest AIC
  topModels = c(which.min(aic))
  
  for (i in 1:(2^(length(predictors))-1)) {
    posteriorProbabilityNumerator <- exp(-0.5*(bic[i]-min(bic)))
    posteriorProbabilityDenominator <- 0
    for (j in 1:(2^(length(predictors))-1)) {
      posteriorProbabilityDenominator <- posteriorProbabilityDenominator + exp(-0.5*(bic[j]-min(bic)))
    }
    posteriorProbabilityVector[i] = posteriorProbabilityNumerator/posteriorProbabilityDenominator
  }
  result$posterior_probability = posteriorProbabilityVector
  result$mod_formula = mod_formula
  
  # create all model prediction for last 6 months
  #prediction <- data.frame(predict(all_models_results[[toString(m)]],googleDataModelTesting))
  modelPrediction <- matrix(NaN, nrow = nrow(googleDataModelTesting), ncol = (2^(length(predictors))-1))
  for (m in 1:(2^(length(predictors))-1)){
    modelPrediction[,m] <- c(predict(all_models_results[[toString(m)]], googleDataModelTesting))
  }
  
  #This code is only used when doing a model average of select few models
  # create histogram to see probable vs non-probable
  # histName = paste(state, '|', 2^(length(predictors))-1, 'total models')
  # h <- hist(result$posterior_probability,xlab="Posterior Probability",ylab="Frequency", main=histName, axes=TRUE)
  # title(sub = toString(format(quantile(result$posterior_probability, probs = c(.1,.25,.5,.75,.9)),digits=3)))
  # h_breaks <- h$breaks
  # counter <- 0
  
  # figure out which models are lilely models
  # for (i in 1:(2^(length(predictors))-1)) {
  #   if(posteriorProbabilityVector[i] > h_breaks[2]){
  #     counter <- counter + 1
  #     topModels <- c(topModels,mod_number[i])
  #   }
  # }

  # plotName = paste(state, '|', counter, 'probable |', 2^(length(predictors))-1, 'total models')
  # plot(sort(posteriorProbabilityVector,decreasing = TRUE),xlab="Rank",ylab="Probability", main=plotName, axes=TRUE)
  modelAverageVec <- numeric(nrow(googleDataModelTesting))
  topModelsProbSum = 0
  # get sum of topModels posterior probabilites to normalize them (sum to 1)
  for (j in topModels){
    topModelsProbSum = topModelsProbSum + posteriorProbabilityVector[j]
  }
  for (j in topModels){
    modelAverageVec <- modelAverageVec + posteriorProbabilityVector[j]/topModelsProbSum*modelPrediction[,j]
  }
  modelAverage[state] <- modelAverageVec
}
dev.off()
closeAllConnections()

write.table(modelAverage, file="estimates_topAIC_models.csv", sep=',', row.names = F)#, col.names = statesList)