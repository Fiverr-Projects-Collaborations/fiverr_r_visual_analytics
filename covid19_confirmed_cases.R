# The COVID-19 outbreak was first identified in December 2019 in Wuhan, China. The WHO declared the outbreak a Public Health Emergency of International Concern on 30 January 2020 and a pandemic on 11 March (Wikipedia). 
# Organizations worldwide have been collecting data so that the government can monitor and learn from this pandemic. You will use the dataset ‘time_series_covid_19_confirmed.csv’ from LMS to explore the COVID-19 data.
# Note: This data set details can be found via https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset#time_series_covid_19_confirmed.csv;
# Your data analysis should include but not limited to the answers to the following questions:
#   1. Create two graphs that displays the latest number of COVID-19 cases of the top 10 and bottom 10 countries, respectively. Consider how to improve the quality and aesthetics of your visualization. 
#   2. Visualize the confirmed cases worldwide from January to March. 
#   3. Visualize the confirmed cases of COVID-19 in China and the rest of the world from January to March. Can you relate the main changes observed from the plot with the landmark events such as WHO declared a pandemic?
#   4. Add a smooth trend line using linear regression to measure how fast the number of cases is growing in China after 15 February 2020. How does the rest of the world compare to linear growth? 
#   5. Raise at least one question from your own regarding the COVID-19 pandemic and find answers using the given dataset. 

#install.packages('plotly')
#install.packages("wordcloud2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")
#install.packages("lubridate")
library(lubridate)
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)
library(wordcloud2) 
covid <- data.frame(read.csv(file='time_series_covid_19_confirmed.csv', header = TRUE))
covid
########### Q5 part 1 ############
covid_latest <- select(covid, Country.Region, X3.23.20)
covid_latest
total_covid_latest <- aggregate(covid_latest$X3.23.20, by=list(Category=covid_latest$Country.Region), FUN=sum)
total_covid_latest<-total_covid_latest[!is.na(total_covid_latest$x),]
covid_top10_latest <- droplevels(head(total_covid_latest[order(total_covid_latest$x, decreasing = TRUE),],10))
covid_bottom10_latest <- droplevels(tail(total_covid_latest[order(total_covid_latest$x,decreasing = TRUE),],10))
# top 10
covid_top10_latest
fig_top <- plot_ly(covid_top10_latest, x = covid_top10_latest$Category, y = covid_top10_latest$x, type = 'bar')
fig_top <- fig_top %>% layout(title = 'Top 10 Countries with latest number of Covid-19 Cases',
                      yaxis = list(zeroline = FALSE),
                      xaxis = list(zeroline = FALSE))
fig_top

# Bottom 10
covid_bottom10_latest
fig <- plot_ly(
  x = covid_bottom10_latest$Category,
  y = covid_bottom10_latest$x,
  name = "Bottom 10 Active",
  type = "bar"
)
fig
# no actual findings from this since all have 0 cases, so we may use wordcloud to see the 
# list of countries

wordcloud2(data=data.frame(name=covid_bottom10_latest$Category, freq=rep(1,10)),minRotation = -pi/4,
           maxRotation = pi/4,rotateRatio = 0.75, widgetsize = NULL,
           figPath = NULL,size=0.25,hoverFunction = NULL)


####### Q5 Part 2 ####### 
covid
data <- covid %>% select(-c(1:4)) %>% gather("date","cases",1:62) 
data$date <- str_sub(data$date, 2) %>% mdy()
data
data<-aggregate(data$cases, by=list(category=data$date), FUN=sum)
plot_ly(data, x = ~category, y = ~x, type = 'scatter', mode='lines+markers') %>%
  layout(title = "Confirmed Cases Worldwide from January to March", xaxis = list(title = "Date"), yaxis = list(title = "Confirmed Cases"))



####### Q5 Part 3 ####### 

covid
data <- covid %>% select(-c(1,3,4)) %>% gather("date","cases",2:63) 
data$date <- str_sub(data$date, 2) %>% mdy()
data
data<-aggregate(data$cases, by=list(category=data$Country.Region,data$date), FUN=sum)
plot_ly(data, x = ~Group.2, y = ~x, type = 'scatter', mode = 'lines', color = data$category) %>%
  layout(title = "Confirmed Cases Country-wise from January to March", xaxis = list(title = "Date"), yaxis = list(title = "Confirmed Cases"))

# We observe that when The WHO declared the outbreak a Public Health Emergency of International Concern 
# on 30 January 2020, the cases were seen rising exponentially in most of the countries including China.
# However, the cases were becoming stable in China when it was declared a pandemic on 11 March.

##### Q5 Part 4 ##### 

covid
china <- covid %>% filter(Country.Region=="China") %>%
  select(-c(1:4)) %>%
  gather("date","cases",1:62) 
china$date <- str_sub(china$date, 2) %>% mdy()
china<-aggregate(china$cases, by=list(category=china$date), FUN=sum)
china<-droplevels(china[china$category>'2020-02-15',])
china
glimpse(china)
model <- lm(log(x)~category, data = china)
summary(model)
test <- data.frame(category = seq(ymd('2020-02-15'),ymd('2020-03-22'),by='day'))
test
test$preds <- exp(predict(model, newdata=test))
test %>% ggplot(aes(x=category, y=preds)) +
  geom_line() +
  scale_y_log10() +
  labs(title = "Cases in China after 15th Feb 2020",
       x="",
       y="Log Scale 10") +
  geom_point(aes(x=category,y=x), data=china, col="red")

##### Q5 Part 5 ##### 
#Top 10 countries with highest increase in cases from Jan to March
covid
data <- covid %>% select(c(2,'X1.31.20'))
data2 <- covid %>% select(c(2,'X3.22.20'))
data<-aggregate(data$X1.31.20, by=list(category=data$Country.Region), FUN=sum)
data2<-aggregate(data2$X3.22.20, by=list(category=data2$Country.Region), FUN=sum)
data$March<-data2$x
data$Jan<-data$x
data$x=NULL
data$increase <- data$March-data$Jan
data<-data[order(-data$increase),] 
data<-droplevels(head(data,10))
plot_ly(data, x = ~category, y = ~increase, type = 'bar') %>%
  layout(title = "Top 10 Countries with Covid-19 cases Increase from Jan to March", xaxis = list(title = "Date"), yaxis = list(title = "Confirmed Cases"))
