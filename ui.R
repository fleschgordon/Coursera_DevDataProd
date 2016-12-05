#
# This is the user-interface definition of a Shiny web application. 
#

library(shiny)
library(RSQLite)
library(dplyr)
library(plotly)

# Connect to data base ----------------------------------------------------
con <- dbConnect(SQLite(), dbname="database.sqlite")
League <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
League <- select(League, country_id, name) %>% rename(league_name = name) # use country_id as key for join
Seasons <- tbl_df(dbGetQuery(con,"SELECT Distinct season FROM Match"))
dbDisconnect(con)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Socker Teampoints Heatmap"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            helpText("This application shows a heatmap of points for sockerteams, based on their reached points per socker season.")
             ,selectInput('selSeason', 'Soccer Season', Seasons$season, selected=c("2015/2016","2014/2015","2013/2014"), multiple = TRUE)
             ,selectInput('selLeague', 'Soccer League', League$league_name, selected=c("Germany 1. Bundesliga"), multiple = FALSE)
             
    ),
    
    # Show the Heatmap
    mainPanel(
            plotlyOutput("Heatmap")
    )
  )
))
