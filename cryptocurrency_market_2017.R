# After Bitcoin was launched in 2009, hundreds of similar projects based on the blockchain technology have emerged. Currently, Bitcoin is the world’s largest cryptocurrency by market capitalization. Meanwhile, the cryptocurrency market is exceptionally volatile and can make you lose all the money easily. You will use the dataset ‘cryptocurrency_market_2017.csv’ from LMS to explore the bitcoin cryptocurrency market. 
# Your data analysis should include but not limited to the answers to the following questions:
#   1. Are there any cryptocurrencies listed in this dataset that have no known market capitalization (market_cap_usd)? If yes, they can be removed from the dataset. 
#   2. Bitcoin has the largest market capitalization. Let’s compare Bitcoin with the rest of the cryptocurrencies. You can visualize the percentage of market capitalization for the top 10 coins as a barplot. Consider how to improve the quality and aesthetics of your visualization. 
#   3. Let’s explore the volatility of the cryptocurrencies market. You can select and plot the top 10 (sort by percent_change_24h in ascending order) coins’ 1 hour (percent_change_1h), 24 hours (percent_change_24h) and 7 days percentage change (percent_change_7d). 
#   4. Design the bar plots to best display the daily and weekly biggest gainers and the biggest losers in market capitalization. 
#   5. Raise at least one question from your own regarding the bitcoin cryptocurrency market and find answers using the given dataset. 

#install.packages("dplyr")
#install.packages('plotly')
library(plotly)
library(dplyr) 

#Load bitcoin data into dataframe raw_bitcoin dataframe
raw_bitcoin <- read.csv("cryptocurrency_market_2017.csv",header = TRUE)


##### Q6 Part 1 ##### 
## Are there any cryptocurrencies listed in this dataset that have no known market capitalization (market_cap_usd)? 
sum(is.na(raw_bitcoin$market_cap_usd))
## 295 such cryptocurrencies listed in this dataset that have no known market capitalization

##If yes, they can be removed from the dataset
data <- raw_bitcoin[!(is.na(raw_bitcoin$market_cap_usd)),]
df <- data[,c('id','market_cap_usd')]
df

##### Q6 Part 2 ##### 
# Bitcoin has the largest market capitalization. 
# Let's compare Bitcoin with the rest of the cryptocurrencies. 
# You can visualize the percentage of market capitalization for the top 10 coins as a barplot. 
# Consider how to improve the quality and aesthetics of your visualization

df <- arrange(df, desc(market_cap_usd))
cap10 <- slice(df, 1:10)
cap10 <- mutate(cap10, market_cap_perc = (cap10$market_cap_usd/(sum(cap10$market_cap_usd))) * 100 )
cap10 <- droplevels(cap10)
fig <- plot_ly(
  x = cap10$id,
  y = cap10$market_cap_perc,
  name = "Top 10 market capitalization",
  type = "bar"
)

fig <- plot_ly(cap10, x = ~id, y = ~market_cap_perc, type = 'bar') %>%
  layout(title = "Top 10 market capitalization", xaxis = list(title = "id"), yaxis = list(title = "Market Cap %"))
fig

##### Q6 Part 3 ##### 
# Let's explore the volatility of the cryptocurrencies market. 
# You can select and plot the top 10 
# (sort by percent_change_24h in ascending order) coins' 
# 1 hour (percent_change_1h), 24 hours (percent_change_24h) and 
# 7 days percentage change (percent_change_7d). 

volatility <- data[,c('id', 'percent_change_24h', 'percent_change_1h','percent_change_7d')]
volatility <- volatility[!(is.na(volatility$percent_change_24h)),]
volatility <-  volatility[order(volatility$percent_change_24h),]
vol10 <- slice(volatility, 1:10)

fig1 <- plot_ly(vol10, x = ~id, y = ~percent_change_1h, type = "bar", name='Hourly') 
fig2 <- plot_ly(vol10, x = ~id, y = ~percent_change_24h, type = "bar", name = 'Daily')
fig3 <- plot_ly(vol10, x = ~id, y = ~percent_change_7d, type = "bar", name = 'Weekly') 
fig <- subplot(fig1, fig2, fig3)
fig <- fig %>%
  layout(title = "% change hourly/daily/weekly", xaxis = list(title = "id"), yaxis = list(title = "% change"))
fig

##### Q6 Part 4 ##### 
# Design the bar plots to best display the daily and weekly biggest gainers and 
# the biggest losers in market capitalization. 

fig1 <- plot_ly(head(volatility, n= 10), x = ~id, y = ~percent_change_24h, type = "bar",name='Top 10')
fig2 <- plot_ly(tail(volatility, n= 10), x = ~id, y = ~percent_change_24h, type = "bar",name='Bottom 10')
fig <- subplot(fig1, fig2)
fig <- fig %>% layout(title = "24 hours top losers and winners",
                      xaxis = list(title = 'id'),
                      yaxis = list(title = '% change 24 hours'))
fig

fig1 <- plot_ly(head(volatility, n= 10), x = ~id, y = ~percent_change_7d, type = "bar",name='Top 10')
fig2 <- plot_ly(tail(volatility, n= 10), x = ~id, y = ~percent_change_7d, type = "bar",name='Bottom 10')
fig <- subplot(fig1, fig2)
fig <- fig %>% layout(title = "Weekly top losers and winners",
                      xaxis = list(title = 'id'),
                      yaxis = list(title = '% change 7 days'))
fig

##### Q6 Part 5 ##### 

# 5. Raise at least one question from your own regarding the bitcoin cryptocurrency market and 
# find answers using the given dataset
# Question: Number of cryptocurrencies with their market capitalizations
# We will divide coins in three ranges 
# 1. market_cap_usd > 50000000 (biggish)
# 2. market_cap_usd <= 50000000 & market_cap_usd > 300000000 (micro)
# 3. market_cap_usd <= 300000000 (nano)


max(df$market_cap_usd)
mean(df$market_cap_usd)
min(df$market_cap_usd)

biggish <- df[df$market_cap_usd > 50000000,]
micro <- df[df$market_cap_usd <= 50000000 & df$market_cap_usd > 10000000,]
nano <- df[df$market_cap_usd <= 10000000,]

market_cap_usd_count <- c(length(biggish$id), length(micro$id), length(nano$id))
market_cap_usd_category <- c("biggish","micro","nano")

df_hist = data.frame(market_cap_usd_category, market_cap_usd_count)

fig <- plot_ly(df_hist, x = ~market_cap_usd_category, y = ~market_cap_usd_count, type = "bar")
fig%>% layout(title = "Market Capitalization of CryptoCurrency",
              xaxis = list(title = 'Market Cap USD Category'),
              yaxis = list(title = 'Market Cap USD Count'))
