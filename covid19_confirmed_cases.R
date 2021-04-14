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



# analysis 2
######################## QUESTION 5 ########################

# install.packages('ggplot2')
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("dplyr")

library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(dplyr) 

covid_data <- data.frame(read.csv2(file='time_series_covid_19_confirmed.csv' ,header = TRUE, sep = ','))

#####################-------------- 5.1 ---------------########################
# Create two graphs that displays the latest number of COVID-19 cases of the top 10 and
# bottom 10 countries, respectively.
# Consider how to improve the quality and aesthetics of your visualization


########## Latest data given in the file is present for 27th Feb 2021 ######################
########## Select feb 27 data for all the country and store into a dataframe new_confirmed_cases

new_confirmed_cases <- select(covid_data, Country.Region, X3.23.20)

######### Renaming column names ######################
colnames(new_confirmed_cases)[1] <- "Country_Region"
colnames(new_confirmed_cases)[2] <- "Feb_27_21"


## Grouping by countring and summing up the number of confirmed cases 
total_new_cases <- aggregate(new_confirmed_cases$Feb_27_21, by=list(Category=new_confirmed_cases$Country_Region), FUN=sum)

## Dropping null values
total_new_cases<-total_new_cases[!is.na(total_new_cases$x),]

## Sorting dataframe by the number of confirmed cases in descending order

## Top 10 
covid_top10 <- droplevels(head(total_new_cases[order(total_new_cases$x, decreasing = TRUE),],10))

## Bottom 10
covid_bottom10 <- droplevels(tail(total_new_cases[order(total_new_cases$x,decreasing = TRUE),],10))

## Scatter plot to show Top 10 country with number of confirmed cases 
ggplot(covid_top10, aes(x = Category, y = x)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Country", y = "No. of Cases", 
       title = "Latest Number of COVID-19 Cases (TOP - 10)")

## Scatter plot to show Bottom 10 country with number of confirmed cases 
ggplot(covid_bottom10, aes(x = Category, y = x)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Country", y = "No. of Cases", 
       title = "Latest Number of COVID-19 Cases (BOTTOM - 10)")

#####################-------------- 5.2 ---------------########################
# Visualize the confirmed cases worldwide from January to March
covid_data
## Pivoting data, columns into row and create date and number of confirmed cases 
df <- covid_data[,-c(1:4)]

#rows to columns
df <- gather(df, "date","cases",1:62)
df <- df[!is.na(df$cases),]
## Date conversion
df$date <- mdy(str_sub(df$date, 2))
df
## Aggregating data by date
df<-aggregate(df$cases, by=list(category=df$date), FUN=sum)
## Bar plot to show Confirmed Cases Worldwide from: January to March
ggplot(data=df, aes(x=category, y=x)) + geom_bar(stat="identity", color="black", width=0.5,fill="steelblue")+theme_minimal()+
  labs(title = "Confirmed Cases Worldwide from: January to March",x= 'Date', y = 'Confirmed Cases') +
  scale_y_continuous(labels = scales::comma)

#####################-------------- 5.3 ---------------########################
# Visualize the confirmed cases of COVID-19 in China and the rest of the world from January to March. 
# Can you relate the main changes observed from the plot with the landmark events 
# such as WHO declared a pandemic?


### Dicing dataframe to select data only from January to March 2020.
df <- covid_data 
df <- gather(df, "date","cases",5:66)
df <- df[!is.na(df$cases),]
## Date conversion
df$date <- mdy(str_sub(df$date, 2))
df

## Aggregating data by Date and Country/Region
df<-aggregate(df$cases, by=list(category=df$Country.Region,df$date), FUN=sum)

## Line chart to show Confirmed Cases of COVID-19 in China vs Rest of the World
df %>% 
  mutate(grouped_data = if_else(category == 'China', 'China', 'Not China')) %>%
  group_by(grouped_data, Group.2) %>% 
  summarise(confirmed = sum(x)) %>% 
  ggplot(aes(x = Group.2, y = confirmed)) +
  geom_line(aes(color = grouped_data)) +
  scale_color_manual(values = c('navy', 'red')) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Confirmed Cases of COVID-19 in China vs Rest of the World",x= 'Date', y = 'Confirmed Cases')+
  theme(legend.title=element_blank())

#####################-------------- 5.4 ---------------########################
# Add a smooth trend line using linear regression to measure how fast the number of cases is growing in China 
# after 15 February 2020. 
# How does the rest of the world compare to linear growth? 
  

### Dicing dataframe to select data only after 15 February 2020
covid_data
df_china <- covid_data %>% 
  filter(Country.Region=="China") %>% 
  gather("date","cases",29:66) %>% 
  select("date","cases")
df_other <- covid_data %>% 
  filter(Country.Region!="China") %>% 
  gather("date","cases",29:66) %>% 
  select("date","cases")

df_china$date <- str_sub(df_china$date, 2) %>% mdy()
df_china<- setNames(aggregate(df_china$cases, by=list(category=df_china$date), FUN=sum), c("date","cases"))
## Dropping null values.
df_china<-df_china[!is.na(df_china$cases),]

df_other$date <- str_sub(df_other$date, 2) %>% mdy()
df_other<- setNames(aggregate(df_other$cases, by=list(category=df_other$date), FUN=sum), c("date","cases"))
df_other<-df_other[!is.na(df_other$cases),]


## LR model
model_predict <- lm(log(cases)~date, data = df_china)
summary(model_predict)

data_df <- data.frame(category = seq(ymd('2020-02-15'),ymd('2020-03-23'),by='day'))
data_df
data_df$preds <- exp(predict(model_predict))

## Visual to show Cases in China After 15th Feb 2020 vs LR line
data_df %>% ggplot(aes(x=category, y=preds)) +
  geom_line() +
  labs(title = "Trendline for China vs Rest of the World", x = 'Date', y='Number of Cases') +
  geom_line(aes(x=date,y=cases), data=df_china, col="Blue") +
  geom_line(aes(x=date,y=cases), data=df_other, col="Red") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.title=element_blank())



#####################-------------- 5.5 ---------------########################
# Raise at least one question from your own regarding the COVID-19 pandemic and 
# find answers using the given dataset. 
#====================
# Majorly Affected COUNTRIES

# This gives us a quick way to compare the growth of covid between a few different countries.
#====================

## Filtering out the data from countries only for, "US","China","Italy","Spain","France"
imp_country <- covid_data %>% 
  filter(Country.Region %in% c("US","China","Italy","Spain","France")) %>% 
  gather("date","cases",5:66) %>% 
  select("Country.Region","date","cases")

imp_country$date <- str_sub(imp_country$date, 2) %>% mdy()

## Visual to show over all growth of Covid between countries
imp_country %>% 
  group_by(Country.Region, date) %>% 
  summarise(confirmed = sum(cases)) %>% 
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(aes(color = Country.Region)) +
  scale_color_manual(values = c('navy', 'orange','green', 'purple', 'red'))+
  labs(title = "Growth of Covid between Countries", x = 'Date', y='Number of Cases') +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.title=element_blank())



