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


points_diff <- data.frame()
for (year in unique(df$Season)) {

    t_df <- df %>% filter(Season==year)

    #get home winnging percentage for this year
    
    #create points tracking
    teams = length(unique(t_df$home))
    pts <- integer(teams)
    names(pts) <- unique(t_df$home)


    for (i in 1:nrow(t_df)) {

        #Check if the teams are close in the standings
        row = t_df[i,]
        #Check if the home team won
        if (row$hgoal > row$vgoal) {
            points_diff <- rbind(points_diff, c(abs(pts[row$home] - pts[row$visitor]), 1))
            pts[row$home] = pts[row$home] + 3
        } else if (row$hgoal < row$vgoal) {
            points_diff <- rbind(points_diff, c(abs(pts[row$home] - pts[row$visitor]), 0))
            pts[row$visitor] = pts[row$visitor] + 3
        } else {
            points_diff <- rbind(points_diff, c(abs(pts[row$home] - pts[row$visitor]), 0))
            pts[row$visitor] = pts[row$visitor] + 1
            pts[row$home] = pts[row$home] + 1
        }

    }

}


names(points_diff) <- c("points_diff", "hw")
points_diff <- 
    points_diff %>% group_by(points_diff) %>%
    summarize(percent=sum(hw)/n()) %>%
    filter(points_diff < 20)

plot(points_diff, xlab="Point Difference", ylab="Home Winning Percentage")

