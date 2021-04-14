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





# ANALYSIS 2


######################## QUESTION 6 ########################


#install.packages("ggpubr")
library(dplyr) 
library(ggplot2)
library(tidyr)
library(ggpubr)
library(stringr)
crypto <- read.csv("cryptocurrency_market_2017.csv",header = TRUE)
crypto
str(crypto)

#6.1
nrow(crypto[is.na(crypto$market_cap_usd),])
# 295 cryptocurrencies that have no known market capitalization
data <- crypto[!(is.na(crypto$market_cap_usd)),]
data

#6.2
top_10 <- head(arrange(data, desc(market_cap_usd)),10)
top_10
top_10$mcap_perc <- top_10$market_cap_usd/(sum(top_10$market_cap_usd)) * 100 

ggplot(top_10, aes(x=id, y=mcap_perc, fill= mcap_perc))+ 
  geom_bar(stat = "identity") +  theme(axis.text.x=element_text(angle = 60, vjust = 0.8))+
  ggtitle("Top 10 Cryptocurrency w.r.t. Market Capitalization") +
  xlab("Cryptocurrency") + ylab("Market Capitalization in USD") + labs(fill = "% Market Cap")


#6.3
top_10_volatile<-head(arrange(data, percent_change_24h),10)
top_10_volatile<-top_10_volatile %>% gather(change_type, percentage, c(percent_change_1h,percent_change_24h,percent_change_7d))

ggplot(top_10_volatile,                                      
       aes(x = id,
           y = percentage,
           fill = change_type)) +coord_flip()+
  geom_bar(stat = "identity",
           position = "dodge")+
  ggtitle("Cryptocurrency Volatility ") +
  xlab("Cryptocurrency") + ylab("% Change") + labs(fill = "% Change Type")


# 6.4
data_24h<-data[!is.na(data$percent_change_24h),]
top_10_volatile_daily<-head(arrange(data_24h, -percent_change_24h),10)
bottom_10_volatile_daily<-head(arrange(data_24h, percent_change_24h),10)
data_7d<-data[!is.na(data$percent_change_7d),]
data_7d$id <- str_wrap(data_7d$id, width = 15)
top_10_volatile_weekly<-head(arrange(data_7d, -percent_change_7d),10)
bottom_10_volatile_weekly<-head(arrange(data_7d, percent_change_7d),10)

# Biggest 10 Daily gainers
p1 <- ggplot(top_10_volatile_daily, aes(x=id, y=percent_change_24h, fill= percent_change_24h))+ 
  geom_bar(stat = "identity") +  theme(axis.text.x=element_text(angle = 60, vjust = 0.8))+
  ggtitle("Biggest 10 Daily Gainers") + xlab("Cryptocurrency")+ ylab("% Change") + labs(fill = "% Change")

p2 <- ggplot(bottom_10_volatile_daily, aes(x=id, y=percent_change_24h, fill= percent_change_24h))+ 
  geom_bar(stat = "identity") +  theme(axis.text.x=element_text(angle = 60, vjust = 0.8))+
  ggtitle("Biggest 10 Daily Losers") +xlab("Cryptocurrency")+ylab("% Change") + labs(fill = "% Change")

p3 <- ggplot(top_10_volatile_weekly, aes(x=id, y=percent_change_24h, fill= percent_change_24h))+ 
  geom_bar(stat = "identity") +  theme(axis.text.x=element_text(angle = 60, vjust = 0.8))+
  ggtitle("Biggest 10 Weekly Gainers") +
  xlab("Cryptocurrency") + ylab("% Change") + labs(fill = "% Change")

p4 <- ggplot(bottom_10_volatile_weekly, aes(x=id, y=percent_change_24h, fill= percent_change_24h))+ 
  geom_bar(stat = "identity") +  theme(axis.text.x=element_text(angle = 60, vjust = 0.8))+
  ggtitle("Biggest 10 Weekly Losers") + coord_flip()+
  xlab("Cryptocurrency") + ylab("% Change") + labs(fill = "% Change")


ggarrange(p1, p2, p3, p4 + rremove("x.text"), 
          labels = c("I", "II", "III", "IV"),
          ncol = 2, nrow = 2)



#6.5 

# categorizing cryptocurrencies as large-cap, mid-cap & small-cap
# https://blockgeeks.com/guides/cryptocurrency-market-cap/
# large-cap => market_cap > $10B
# mid-cap => market_cap between $1B and $10B
# small-cap => market_cap < $1B
data_categorize<-data
data_categorize$market_type<-ifelse(data_categorize$market_cap_usd > 10000000000,"Large-Cap", ifelse(data_categorize$market_cap_usd > 1000000000,"Mid-Cap","Small-Cap"))
data_categorize$market_type
quantile(data$market_cap_usd, c(0.1, 0.9))
g1<-ggplot(data_categorize[data_categorize$market_type=='Small-Cap',], aes(x=market_type, y=market_cap_usd, fill=market_type)) + 
  geom_boxplot(alpha=0.75) + coord_flip() +
  theme(legend.position="none") +                                          
  geom_point() +
  scale_fill_brewer(palette="BrBG") +
  xlab("Market Type") + ylab("Market Capitalization in USD") + theme(legend.position="top")+labs(fill = "Market Type")

g2<-ggplot(data_categorize[data_categorize$market_type=='Mid-Cap',], aes(x=market_type, y=market_cap_usd, fill=market_type)) + 
  geom_boxplot(alpha=0.75) + coord_flip() +
  theme(legend.position="none") +                                          
  geom_point() +
  scale_fill_brewer(palette="RdBu") +
  xlab("Market Type") + ylab("Market Capitalization in USD") + theme(legend.position="top")+labs(fill = "Market Type")

g3<-ggplot(data_categorize[data_categorize$market_type=='Large-Cap',], aes(x=market_type, y=market_cap_usd, fill=market_type)) + 
  geom_boxplot(alpha=0.75) + coord_flip() +
  theme(legend.position="none") +                                          
  geom_point() +
  scale_fill_brewer(palette="Dark2") +
  xlab("Market Type") + ylab("Market Capitalization in USD") + theme(legend.position="top")+labs(fill = "Market Type")

g4<-ggplot(data_categorize, aes(x=market_type, y=market_cap_usd, fill=market_type)) + 
  geom_boxplot(alpha=0.75) + coord_flip() +
  theme(legend.position="none") +                                          
  geom_point() +
  scale_fill_brewer(palette="PiYG") +
  xlab("Market Type") + ylab("Market Capitalization in USD") + theme(legend.position="top")+labs(fill = "Market Type")

ggarrange(g1, g2, g3, g4 + rremove("x.text"), 
          labels = c("I", "II", "III", "IV"),
          ncol = 2, nrow = 2)
