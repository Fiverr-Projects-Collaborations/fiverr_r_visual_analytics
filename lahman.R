# 2. Use the Lahman package and the Teams data frame to answer the following questions: 
#   2.1 Define two new variables in the Teams data frame: batting average (BA) and slugging percentage (SLG). Batting average is the ratio of hits (H) to at-bats (AB), and slugging percentage is total bases divided by at-bats. To compute total bases, you get 1 for a single, 2 for a double, 3 for a triple, and 4 for a home run.
#   2.2 Plot a time series of SLG since 1954 conditioned by lgID. Is slugging percentage typically higher in the American League (AL) or the National League (NL)?
#   2.3 Display the top 15 teams ranked in terms of slugging percentage in MLB history. Repeat this using teams since 1969.
#   2.4 Create a factor called election that divides the yearID into four-year blocks that correspond to U.S. presidential terms (from 1788 to 2017). During which term have the most home runs been hit? (Hint: seq function)

#install.packages('Lahman')
# install.packages('ggplot2')
library('Lahman')
library(plotly)
library(dplyr)
library(ggplot2)
teams <- Lahman::Teams

########### Q2 Part 1 ########### 

#Calculating BA and SLG and storing into a dataframe
teams <- mutate(teams, BA = teams$H / teams$AB)
teams <- mutate(teams, SLG = (teams$H + (2 * teams$X2B) + (3 * teams$X3B) + (4 * teams$HR)) / teams$AB)
teams

########### Q2 Part 2 ########### 

time_ser <- teams[,c('yearID','lgID','SLG')][teams$lgID %in% c('AL','NL') & teams$year>1954,]
#Plotting into graph
fig1 <- plot_ly(time_ser,  x= ~yearID, y = ~SLG, mode = 'lines', color = ~lgID)
fig1
# slugging percentage typically higher in the American League (AL) 

########### Q2 Part 3 ########### 

#Filter data for top 15 team in all the history
top_15_history <- head(teams[order(teams$SLG,decreasing = TRUE),],15)

#Filter data for top 15 teams since 1969
df_1969 <- filter(teams, yearID > 1969)
top_15_1969 <- head(arrange(df_1969, desc(SLG)),15)

fig1 <- plot_ly(x = top_15_history$name, y = top_15_history$SLG, type = 'bar', name = 'top_15_history')
fig2 <- plot_ly(x = top_15_1969$name, y = top_15_1969$SLG, type = 'bar', name = 'top_15_1969')
fig <- subplot(fig1, fig2)
fig%>% layout(title = 'Top ranked teams',
              yaxis = list(title = 'SLG'),
              xaxis = list(title = 'Teams'))

#Dividing yearID into four years block to correspond US presendtial terms.
yearBreaks <- seq(1788,2017, by = 4)
yearBreaks
TeamElectionYears <-
  teams %>% 
  mutate(election = 
           cut(yearID, breaks = yearBreaks, right = FALSE))

ggplot(TeamElectionYears, aes(x = election, y = HR)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "US Presidential Terms", y = "Home Runs Hit", 
       title = "Home Runs v. US Presidential Years")
# We can see that the US presidential term with the most homeruns hit is 1996 to 2000
