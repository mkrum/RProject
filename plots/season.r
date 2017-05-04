#! /usr/local/bin/Rscript

library(engsoccerdata)
library(dplyr)

df <- spain %>% filter(tier==1)


#Season, Home Team, time of year
df <- df %>%
    select(Date, Season, home, visitor, hgoal, vgoal) %>%
    #1 = Home wins, 0 = tie/loss
    mutate(GD=hgoal-vgoal,
                result = ifelse(GD > 0, 1, 0)
           ) %>%
    filter(length(Date) > 1)


d_df <- data.frame()

Seasons = length(unique(df$Season))
home_percent <- integer(Seasons)
names(home_percent) <- unique(df$Season)


hp <- data.frame()
for (year in unique(df$Season)) {

    t_df <- df %>% filter(Season==year)

    #get home winnging percentage for this year
    
    home_wins = 0
    games = 0

    for (i in 1:nrow(t_df)) {

        row = t_df[i,]

        games = games + 1 
        #Check if the home team won
        if (row$hgoal > row$vgoal) {
            home_wins = home_wins + 1
        } 

    }
    hp <- rbind(hp, c(year, home_wins/games))
}

plot(hp, xlab="Year", ylab="Home Winning Percentage")


