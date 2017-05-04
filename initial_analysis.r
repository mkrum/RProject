#! /usr/local/bin/Rscript

library(engsoccerdata)
library(dplyr)
options(max.print=100000)



df <- spain %>% filter(tier==1)

df <- df %>%
    select(Date, Season, home, visitor, hgoal, vgoal) %>%
    #1 = Home wins, 0 = tie/loss
    mutate(GD=hgoal-vgoal,
                result = ifelse(GD > 0, 1, 0)
           ) %>%
    filter(length(Date) > 1)

home_advantage <- df %>%
    select(result)
    
#raw home winning percentage
sum(home_advantage)/nrow(home_advantage)



home_games <- df %>% select(Season, team=home, opp=visitor, GF=hgoal, GA=vgoal)

home_games$GF <- as.numeric(home_games$GF) #make sure is numeric
home_games$GA <- as.numeric(home_games$GA) #make sure is numeric
home_games <- home_games %>% mutate(GD = GF-GA)

home_games <-
home_games %>% group_by(team) %>%
  summarize(GP = n(),
            goalsF = sum(GF),
            goalsA = sum(GA),
            goaldif = sum(GD),
            W = sum(GD>0),
            Percent = sum(GD>0)/n() * 100
  )

away_games <- df %>% select(Season, team=visitor, opp=home, GF=vgoal, GA=hgoal)

away_games$GF <- as.numeric(away_games$GF) #make sure is numeric
away_games$GA <- as.numeric(away_games$GA) #make sure is numeric
away_games <- away_games %>% mutate(GD = GF-GA)

away_games <-
away_games %>% group_by(team) %>%
  summarize(GP = n(),
            goalsF = sum(GF),
            goalsA = sum(GA),
            goaldif = sum(GD),
            W = sum(GD>0),
            Percent = sum(GD>0)/n() * 100
  )

#Print out best home teams
print(tbl_df(home_games[rev(order(home_games$Percent)),]), n=60)


rhp <- data.frame(team=home_games$team, GP=home_games$GP, percent_increase=(home_games$Percent - away_games$Percent)/away_games$Percent * 100) %>%
    filter(GP >= 100)

#Print out best home teams in order of percent increase over away winning percentage
rhp[rev(order(rhp$percent)),]

