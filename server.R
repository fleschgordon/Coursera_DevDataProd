#
# This is the server logic of a Shiny web application. 
#

library(shiny)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(RSQLite)
library(reshape2)
library(RColorBrewer)
library(gplots)
library(data.table)
library(d3heatmap)
library(viridis)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        # Connect to data base ----------------------------------------------------
        con <- dbConnect(SQLite(), dbname="database.sqlite")
        
        player       <- tbl_df(dbGetQuery(con,"SELECT * FROM player"))
        Match        <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
        Team        <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
        Country        <- tbl_df(dbGetQuery(con,"SELECT * FROM Country"))
        League        <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
        
        # select columns
        
        player  <- select(player,player_api_id, player_name) # use player_api_id as key for join
        Team    <- select(Team, team_api_id, team_long_name, team_short_name) # use team_api_id as key for join
        Country <-select(Country, id, name) %>% rename(country_id = id)  %>% rename(country_name = name)   # use country_id as key for join
        League  <- select(League, country_id, name) %>% rename(league_name = name) # use country_id as key for join
        Match   <-select(Match, id, country_id, league_id, season, stage, date, match_api_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, home_player_6, home_player_7, home_player_8, home_player_9, home_player_10, home_player_11, away_player_1, away_player_2, away_player_3, away_player_4, away_player_5, away_player_6, away_player_7, away_player_8, away_player_9, away_player_10, away_player_11, goal, shoton, shotoff, foulcommit, card, cross, corner, possession)
        
        dbDisconnect(con)
        
        PointsDf <-Match %>%
                select(1:11)  %>%
                mutate(homePoint = if_else((home_team_goal > away_team_goal),3,if_else((home_team_goal == away_team_goal),1,0))) %>%
                mutate(awayPoint = if_else((home_team_goal > away_team_goal),0,if_else((home_team_goal == away_team_goal),1,3)))
        
        tableHomeDt <- PointsDf %>%
                group_by(season, league_id, home_team_api_id) %>%
                summarise(pointsHome = sum(homePoint)) %>%
                ungroup() %>% data.table
        
        keycols = c("season", "league_id", "home_team_api_id" )
        setkeyv(tableHomeDt,keycols)
        
        tableAwayDt <- PointsDf %>%
                group_by(season, league_id, away_team_api_id) %>%
                summarise(pointsAway = sum(awayPoint)) %>%
                ungroup()  %>% data.table
        keycols = c("season", "league_id", "away_team_api_id" )
        setkeyv(tableAwayDt,keycols)
        
        tableHomeAwayDt <- tableHomeDt[tableAwayDt, nomatch=0] %>%
                mutate(points = pointsHome + pointsAway) %>%
                group_by(season, league_id)  %>%
                mutate(rank = min_rank(desc(points)))
        
        tableLong <- tableHomeAwayDt %>%
                left_join(League, by = c("league_id" = "country_id")) %>%
                left_join(Team, by = c("home_team_api_id" = "team_api_id")) %>%
                ungroup() %>%
                select(season, league_name, rank, team_long_name, points)
   
  output$Heatmap <- renderPlotly({
    
           seasonsdata <- subset(tableLong, season %in% input$selSeason)
           seasonsdata$points <- as.factor(seasonsdata$points)
    
           p <- ggplot(filter(seasonsdata, league_name %in% input$selLeague), mapping = aes(x = season, y = team_long_name)) + 
           geom_tile(mapping = aes(fill = points),color="white", size=0.1 ) + facet_grid(league_name~., scales = "free_y") +scale_fill_viridis(discrete=TRUE) + theme(legend.position = "none")  # free y scale to avoid that all clubs are on Y axis in all leagues
           ggplotly(p)
          #p
    
  })
  
})
