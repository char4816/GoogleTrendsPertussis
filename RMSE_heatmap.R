library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(usmap)

setwd('C:/Users/chris/Google Drive/1Chris/1Research/2018 UROP/Scripts/Final_Scripts')

rmse_data <- data.frame(read.csv("RMSE_heatmap.csv", header = T))
colnames(rmse_data) <- c('fips','abb','full','rmse','r.squared')

plot_usmap(data = rmse_data, values = "rmse", lines = "black") + 
  scale_fill_continuous(
    low = "firebrick4", high = "bisque", name = "RMSE"
  ) + theme(legend.position = "right")

plot_usmap(data = rmse_data, values = "r.squared", lines = "black") + 
  scale_fill_continuous(
    low = "firebrick4", high = "bisque", name = "R^2"
  ) + theme(legend.position = "right")
