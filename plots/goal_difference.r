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

goal_diff <- data.frame()
for (year in unique(df$Season)) {

    t_df <- df %>% filter(Season==year)

    #get home winnging percentage for this year

    for (i in 1:nrow(t_df)) {

        row = t_df[i,]

        if (row$hgoal > row$vgoal) {
            goal_diff <- rbind(goal_diff, c(abs(row$hgoal - row$vgoal), 1))
        } else if (abs(row$hgoal - row$vgoal) != 0) {
            goal_diff <- rbind(goal_diff, c(abs(row$hgoal - row$vgoal), 0))
        }

    }

}

names(goal_diff) <- c("goal_diff", "hw")
goal_diff <- 
    goal_diff %>% group_by(goal_diff) %>%
    summarize(percent=sum(hw)/n())

plot(goal_diff, xlab="Goal Difference", ylab="Home Winning Percentage")

