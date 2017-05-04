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


for (year in unique(df$Season)) {

    t_df <- df %>% filter(Season==year)

    end = tail(t_df$Date, n=1)
    t_df$days <- as.numeric(as.Date(as.character(end), format="%Y-%m-%d") - as.Date(as.character(t_df$Date), format="%Y-%m-%d"), units="days")
    t_df <- t_df %>%
            select(days, result) 
    d_df <- rbind(d_df, t_df)
}

d_df <- 
    d_df %>% group_by(floor(days/5)) %>%
    summarize(percent=sum(result)/n())

plot(d_df, xlab="Days Left in the Season", ylab="Home Winning Percentage")

