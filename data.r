#! /usr/local/bin/Rscript

library(engsoccerdata)
library(dplyr)

df <- spain %>% filter(tier==1)

nrow(df)
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


for (year in unique(df$Season)) {

    t_df <- df %>% filter(Season==year)

    #get home winnging percentage for this year
    
    #create points tracking
    teams = length(unique(t_df$home))
    pts <- integer(teams)
    names(pts) <- unique(t_df$home)

    point_diff <- data.frame()  
    goal_diff <- data.frame()

    for (i in 1:nrow(t_df)) {

        #Check if the teams are close in the standings
        row = t_df[i,]

        point_diff <- rbind(point_diff, abs(pts[row$home] - pts[row$visitor]))

        goal_diff <- rbind(goal_diff, abs(row$hgoal - row$vgoal))
        
        #update points 
        if (row$hgoal > row$vgoal) {
            pts[row$home] = pts[row$home] + 3
        } else if (row$hgoal < row$vgoal) {
            pts[row$visitor] = pts[row$visitor] + 3
        } else {
            pts[row$visitor] = pts[row$visitor] + 1
            pts[row$home] = pts[row$home] + 1
        }

    }

    t_df$point_diff <- point_diff[,1]
    t_df$goal_diff <- goal_diff[,1]

    end = tail(t_df$Date, n=1)
    t_df$days <- as.numeric(as.Date(as.character(end), format="%Y-%m-%d") - as.Date(as.character(t_df$Date), format="%Y-%m-%d"), units="days")
    t_df <- t_df %>%
            select(result, Season, days, point_diff, goal_diff) 
    d_df <- rbind(d_df, t_df)
}

save(d_df, file="laligadata.Rda")

