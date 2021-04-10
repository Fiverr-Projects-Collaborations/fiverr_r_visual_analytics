# 1. Use the nycflights13 package and the flights data frame to answer the following questions: 
#   1.1 What month had the highest proportion of canceled flights (the arr_delay variable is NA)? What month had the lowest? Plot the proportion of canceled flights each month and interpret any seasonal patterns.
#   1.2 What plane (specified by the tailnum variable) traveled the most times from New York City airports (JFK, LGA or EWR) in 2013? Plot the number of trips per week.

# install.packages("plotly")
# install.packages('nycflights13')
library('nycflights13')
library(plotly)
library(dplyr)
flights <- nycflights13::flights
flights
str(flights)
summary(flights)
########### Q1 Part 1 ########### 
x<-subset(flights, is.na(arr_delay))
cancelled<-as.data.frame(table(x$month)) 
cancelled$month<- cancelled$Var1
cancelled$Var1=NULL
cancelled
# Highest: February
# Lowest: October

fig_top <- plot_ly(cancelled, x = ~month, y = cancelled$Freq, type = 'bar')
fig_top <- fig_top %>% layout(title = 'Month-wise cancelled flights',
                              yaxis = list(title = "Cancelled Flights", zeroline = FALSE),
                              xaxis = list(title = "Month", zeroline = FALSE))
# Cancelled flights per month
fig_top
# Here, we observe that the during winter season and also during rainy season, there are 
# more cancelled flights than rest of the months.
########### Q1 Part 2 ########### 

flights_ny<-flights[flights$origin %in% c("JFK","LGA","EWR") & flights$year=='2013',]
flights_ny<- flights_ny[!is.na(flights_ny$tailnum),]
flights_ny
flights_ny_summarized<-as.data.frame(flights_ny %>%
  group_by(tailnum) %>%
  summarise(count = n()) %>%
  arrange(desc(count)))
head(flights_ny_summarized)
# planes that travelled the most - N725MQ

flights_ny_N725MQ<-flights_ny[flights_ny$tailnum=='N725MQ',]
flights_ny_N725MQ$week<-format.Date(as.Date(with(flights_ny_N725MQ,paste(year,month,day,sep="-")),"%Y-%m-%d"),"%V")
flights_ny_N725MQ<-as.data.frame(flights_ny_N725MQ %>%
                                       group_by(week) %>%
                                       summarise(count_week = n()) %>%
                                       arrange(desc(count_week)))
flights_ny_N725MQ$week<- as.factor(flights_ny_N725MQ$week)
plot_ly(data = flights_ny_N725MQ, x=~week, y=~count_week, type='bar')%>% layout(title = 'Weekly Trips for N725MQ for Year 2013 from origins: JFK,LGA,EWR',
                                                                                  yaxis = list(title = 'Number of trips'),
                                                                                  xaxis = list(title = 'Week Number'))
