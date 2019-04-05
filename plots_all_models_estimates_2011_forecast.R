# 1/28/19
# setwd("C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts")
# Generate a plot of just the 6 month forecasting for all models for each state

library(readxl)

# Function to extract the overall ANOVA p-value out of a linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

pdf("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/plots_all_models_estimates_2011_forecast.pdf",paper="a4r", width = 11, height = 8.5)
par(mar=c(5,6,4,1)+.1)
statesList = c('US', 'US-AK', 'US-AL', 'US-AR', 'US-AZ', 'US-CA', 'US-CO', 'US-CT', 'US-DC', 'US-DE', 'US-FL', 'US-GA', 'US-HI', 'US-IA', 'US-ID', 'US-IL', 'US-IN', 'US-KS', 'US-KY', 'US-LA', 'US-MA', 'US-MD', 'US-ME', 'US-MI', 'US-MN', 'US-MO', 'US-MS', 'US-MT', 'US-NC', 'US-ND', 'US-NE', 'US-NH', 'US-NJ', 'US-NM', 'US-NV', 'US-NY', 'US-OH', 'US-OK', 'US-OR', 'US-PA', 'US-RI', 'US-SC', 'US-SD', 'US-TN', 'US-TX', 'US-UT', 'US-VA', 'US-VT', 'US-WA', 'US-WI', 'US-WV', 'US-WY')
namesList = c('United States','ALASKA','ALABAMA','ARKANSAS','ARIZONA','CALIFORNIA','COLORADO','CONNECTICUT','District of Colombia','DELAWARE','FLORIDA','GEORGIA','HAWAII','IOWA','IDAHO','ILLINOIS','INDIANA','KANSAS','KENTUCKY','LOUISIANA','MASSACHUSETTS','MARYLAND','MAINE','MICHIGAN','MINNESOTA','MISSOURI','MISSISSIPPI','MONTANA','NORTH CAROLINA','NORTH DAKOTA','NEBRASKA','NEW HAMPSHIRE','NEW JERSEY','NEW MEXICO','NEVADA','NEW YORK','OHIO','OKLAHOMA','OREGON','PENNSYLVANIA','RHODE ISLAND','SOUTH CAROLINA','SOUTH DAKOTA','TENNESSEE','TEXAS','UTAH','VIRGINIA','VERMONT','WASHINGTON','WISCONSIN','WEST VIRGINIA','WYOMING')
summaryRMSE <- matrix(NaN, nrow = length(statesList), ncol = 4)

allModelData_not_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_all_MAXDATES.xlsx", sheet = 1)[1:417,]
topModelData_not_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_top_MAXDATES.xlsx", sheet = 1)[1:417,]
topAICModelData_not_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_topAIC_MAXDATES.xlsx", sheet = 1)[1:417,]
allGtermsModelData_not_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_allGterms_MAXDATES.xlsx", sheet = 1)[1:417,]

allModelData_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_all_corrected_MAXDATES.xlsx", sheet = 1, col_types = "numeric")[1:417,]
topModelData_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_top_corrected_MAXDATES.xlsx", sheet = 1, col_types = "numeric")[1:417,]
topAICModelData_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_topAIC_corrected_MAXDATES.xlsx", sheet = 1, col_types = "numeric")[1:417,]
allGtermsModelData_corrected = read_excel("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-25-2018-model_averaged_estimates_allGterms_corrected_MAXDATES.xlsx", sheet = 1, col_types = "numeric")[1:417,]

# lineColors = c("firebrick3","navy","royalblue2","turquoise2","orange4", "tan3","gold")
# lineColors = c("gray30","red4","red2","orangered","lightcoral","navy","royalblue2","deepskyblue","turquoise2","lavenderblush3")
lineColors = c("gray30","lightcoral","red2","red4","deepskyblue","royalblue2","navy")

model_RMSE_cor <- data.frame(matrix(ncol = 25, nrow = (length(statesList)+1) ))
model_RMSE_cor[,1] <- c(statesList, 'Ave')

colnames(model_RMSE_cor)<-c('state', 
  'fRMSE_allModelData_not_corrected', 'fRMSE_topModelData_not_corrected', 'fRMSE_topAICModelData_not_corrected', 'fRMSE_allGtermsModelData_not_corrected',
  'fRMSE_allModelData_corrected', 'fRMSE_topModelData_corrected', 'fRMSE_topAICModelData_corrected', 'fRMSE_allGtermsModelData_corrected',
  'tRMSE_allModelData_not_corrected', 'tRMSE_topModelData_not_corrected', 'tRMSE_topAICModelData_not_corrected', 'fRMSE_allGtermsModelData_not_corrected', 
  'tRMSE_allModelData_corrected', 'tRMSE_topModelData_corrected', 'tRMSE_topAICModelData_corrected', 'tRMSE_allGtermsModelData_corrected',
  'r2_allModelData_not_corrected', 'r2_topModelData_not_corrected', 'r2_topAICModelData_not_corrected', 'r2_allGtermsModelData_not_corrected',
  'r2_allModelData_corrected', 'r2_topModelData_corrected', 'r2_topAICModelData_corrected', 'r2_allGtermsModelData_corrected')

for (state in statesList){
  
  #Read in the Project Tycho incidence data.
  csvData = read.csv(paste("C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/PTlevel1data/Final_Incidence_Data/FinalNATimeSeries/PT_",state,".csv",sep=''), header = FALSE)
  xx <- rep(csvData[,2], length.out=773) 
  xx[(nrow(csvData)+1):nrow(allModelData_not_corrected)] <- NA 
  tychoData <- data.frame(xx)
  colnames(tychoData) <- c('incidence')

  tychoDataModelTesting <- data.frame(tychoData[1:417,])
  colnames(tychoDataModelTesting) <- c('incidence')
  
  #Read in Google Trends "pertussis" timeseries data
  csvData = read.csv(paste('C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-24-GTdata/',state,'_','pertussis','.csv',sep=''), header = FALSE)[1:417,]
  pertussis_Trends <- data.frame(csvData[,2])
  colnames(pertussis_Trends) <- c('pertussis')
  if(length(unique(csvData[,2]))!=1){
    pertussis_Trends["pertussis"] <- max(tychoDataModelTesting, na.rm=T)*(csvData[,2]-min(csvData[,2]))/(max(csvData[,2])-min(csvData[,2]))
  }
  else{
    pertussis_Trends["pertussis"] <- csvData[,2]
  }
  #Read in Google Trends "whooping cough" timeseries data
  # csvData = read.csv(paste('C:/Users/Chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/10-24-GTdata/',state,'_','whooping cough','.csv',sep=''), header = FALSE)[1:417,]
  # whooping_cough_Trends <- data.frame(csvData[,2])
  # colnames(whooping_cough_Trends) <- c('whooping cough')
  # if(length(unique(csvData[,2]))!=1)
  #   whooping_cough_Trends["whooping cough"] <- max(tychoDataModelTesting, na.rm=T)*(csvData[,2]-min(csvData[,2]))/(max(csvData[,2])-min(csvData[,2]))
  # else
  #   whooping_cough_Trends["whooping cough"] <- csvData[,2]
  

  
  plotData <- data.frame(allModelData_not_corrected[['date']], tychoDataModelTesting[["incidence"]],
                         allModelData_not_corrected[[state]], topModelData_not_corrected[[state]], topAICModelData_not_corrected[[state]],allGtermsModelData_not_corrected[[state]],
                         allModelData_corrected[[state]], topModelData_corrected[[state]], topAICModelData_corrected[[state]], allGtermsModelData_corrected[[state]], 
                         pertussis_Trends['pertussis'], stringsAsFactors=FALSE)
  colnames(plotData) <- (c('Date', 'Project_Tycho', 'allModelData_not_corrected', 'topModelData_not_corrected', 'topAICModelData_not_corrected','allGtermsModelData_not_corrected',
                           'allModelData_corrected', 'topModelData_corrected', 'topAICModelData_corrected', 'allGtermsModelData_corrected', 
                           'pertussis_Trends'))
  
  yrange <- seq(0,max(plotData[,2:ncol(plotData)],na.rm=T),length=5)
  plot(plotData$Project_Tycho[na.rm=T][392:417]~plotData[,1][392:417],type="l",col=lineColors[1],lwd=2.0,xlab="Date",ylab="Incidence per 100,000", main=namesList[which(statesList == state)], cex.main=2.0, cex.lab=2.0, cex.axis=1.75, ylim=c(0,max(plotData[392:417,2:ncol(plotData)],na.rm=T)), tick = F)
  
  lines(plotData[,5][392:417]~plotData[,1][392:417], col=lineColors[2],lwd=1.0, tick = F)
  lines(plotData[,3][392:417]~plotData[,1][392:417], col=lineColors[3],lwd=1.0, tick = F)
  lines(plotData[,4][392:417]~plotData[,1][392:417], col=lineColors[4],lwd=1.0, tick = F)
  lines(plotData[,9][392:417]~plotData[,1][392:417], col=lineColors[5],lwd=1.0, tick = F)
  lines(plotData[,7][392:417]~plotData[,1][392:417], col=lineColors[6],lwd=1.0, tick = F)
  lines(plotData[,8][392:417]~plotData[,1][392:417], col=lineColors[7],lwd=1.0, tick = F)
  
  # lines(plotData[,6]~plotData[,1], col=lineColors[5],lwd=1.0, tick = F)
  # lines(plotData[,10]~plotData[,1], col=lineColors[9],lwd=1.0, tick = F)
  # lines(plotData[,11]~plotData[,1], col=lineColors[10],lwd=0.3, tick = F)
  
  # box()
  
  axis(1,at = plotData[,1], format(plotData[,1],"%m/%d/%Y"), labels =F, tick = F)
  axis(2, at = yrange, tick = F, labels = F)
  op <- par(cex = 0.75)
  legend(x = 'topright', lwd = 3, lty = rep(1, length(lineColors)), bty = "n", legend=c('PT', 'AIC(i*)', 'All Models Average', 'Top Models Average', 'AR(1) AIC(i*)',
                                                                                'AR(1) All Models Average', 'AR(1) Top Models Average'), pch=rep(NA, length(lineColors)), col=lineColors, cex=1.5)

  # #Compute RMSE and R^2 for different model averages
  # state_metrics <- rep(NA, 24)
  # # # #
  # # fRMSE_allModelData_not_corrected
  # state_metrics[1] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - allModelData_not_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # fRMSE_topModelData_not_corrected
  # state_metrics[2] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - topModelData_not_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # fRMSE_topAICModelData_not_corrected
  # state_metrics[3] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - topAICModelData_not_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # fRMSE_allGtermsModelData_not_corrected
  # state_metrics[4] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - allGtermsModelData_not_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # # #
  # # fRMSE_allModelData_corrected
  # state_metrics[5] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - allModelData_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # fRMSE_topModelData_corrected
  # state_metrics[6] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - topModelData_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # fRMSE_topAICModelData_corrected
  # state_metrics[7] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - topAICModelData_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # fRMSE_allGtermsModelData_corrected
  # state_metrics[8] <- sqrt( sum( (tychoDataModelTesting[["incidence"]][392:417] - allGtermsModelData_corrected[[state]][392:417])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]][392:417])) ) 
  # # # #
  # # tRMSE_allModelData_not_corrected
  # state_metrics[9] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - allModelData_not_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) ) 
  # # tRMSE_topModelData_not_corrected
  # state_metrics[10] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - topModelData_not_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # tRMSE_topAICModelData_not_corrected
  # state_metrics[11] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - topAICModelData_not_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # fRMSE_allGtermsModelData_not_corrected
  # state_metrics[12] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - allGtermsModelData_not_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # # #
  # # tRMSE_allModelData_corrected
  # state_metrics[13] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - allModelData_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # tRMSE_topModelData_corrected
  # state_metrics[14] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - topModelData_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # tRMSE_topAICModelData_corrected
  # state_metrics[15] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - topAICModelData_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # tRMSE_allGtermsModelData_corrected
  # state_metrics[16] <- sqrt( sum( (tychoDataModelTesting[["incidence"]] - allGtermsModelData_corrected[[state]])^2 , na.rm = TRUE ) / sum(!is.na(tychoDataModelTesting[["incidence"]])) )
  # # # #
  # # r2_allModelData_not_corrected
  # state_metrics[17] <- summary(lm(tychoDataModelTesting[["incidence"]]~allModelData_not_corrected[[state]]))$adj.r.squared
  # # r2_topModelData_not_corrected
  # state_metrics[18] <- summary(lm(tychoDataModelTesting[["incidence"]]~topModelData_not_corrected[[state]]))$adj.r.squared
  # # r2_topAICModelData_not_corrected
  # state_metrics[19] <- summary(lm(tychoDataModelTesting[["incidence"]]~topAICModelData_not_corrected[[state]]))$adj.r.squared
  # # r2_allGtermsModelData_not_corrected
  # state_metrics[20] <- summary(lm(tychoDataModelTesting[["incidence"]]~allGtermsModelData_not_corrected[[state]]))$adj.r.squared
  # # # #
  # # r2_allModelData_corrected
  # state_metrics[21] <- summary(lm(tychoDataModelTesting[["incidence"]]~allModelData_corrected[[state]]))$adj.r.squared
  # # r2_topModelData_corrected
  # state_metrics[22] <- summary(lm(tychoDataModelTesting[["incidence"]]~topModelData_corrected[[state]]))$adj.r.squared
  # # r2_topAICModelData_corrected
  # state_metrics[23] <- summary(lm(tychoDataModelTesting[["incidence"]]~topAICModelData_corrected[[state]]))$adj.r.squared
  # # r2_allGtermsModelData_corrected
  # state_metrics[24] <- summary(lm(tychoDataModelTesting[["incidence"]]~allGtermsModelData_corrected[[state]]))$adj.r.squared
  # 
  # model_RMSE_cor[which(statesList == state),2:ncol(model_RMSE_cor)] <- state_metrics
}
#compute average for last row
# for (column in 2:ncol(model_RMSE_cor)){
#   model_RMSE_cor[nrow(model_RMSE_cor),column] <- mean(model_RMSE_cor[1:(nrow(model_RMSE_cor)-1),column])
# }
# write.table(model_RMSE_cor, file="10-25-2018-estimates_plots_ALLDATES_RMSE_cor.csv", sep = ",", row.names = FALSE, col.names = TRUE)

dev.off()