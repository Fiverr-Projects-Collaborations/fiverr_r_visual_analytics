# 3. Using the storms data frame from the nasaweather package:
#   Create a scatterplot between wind and pressure, with color being used to distinguish the type of storm. 
#   You might notice there are lots of overlapping data points in the scatterplot due to a comparatively large sample size, How would you improve your visualization?

# install.packages("nasaweather")
library(nasaweather)
library(plotly)
library(dplyr)
storms<-nasaweather::storms
storms
fig <- plot_ly(data = storms, x = ~wind, y = ~pressure, type = 'scatter',mode='markers', color = ~type)
fig
# We can reduce size of dots to increase visibility of each point
fig <- plot_ly(data = storms, x = ~wind, y = ~pressure, type = 'scatter',mode='markers',marker = list(size = 2), color = ~type)
fig
# we can also introduce opacity to the points to increase visibility of overlapped points
fig <- plot_ly(data = storms, x = ~wind, y = ~pressure, type = 'scatter',mode='markers',marker = list(opacity = 0.2,size = 5), color = ~type)
fig
