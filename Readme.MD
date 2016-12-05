## Developing Data Products Course - Course Project: Shiny Application and Reproducible Pitch

This Application was built as part of the coursera course "Developing Data Procuts" as part of the Coursera Data Science Specialization. 

The app is developed as a shiny web app. A Demo is available at the shinyapps.io hosting site : https://fleschgordon.shinyapps.io/SockerTeamHeatmap/

Source code for ui.R and server.R files are available on the GitHub Repro here: https://github.com/fleschgordon/Coursera_DevDataProd

Web Presentation is here: http://rpubs.com/fleschgordon/233049

## Application

The data for the app comes from a famous socker data set from Hugo Mathien:
https://www.kaggle.com/hugomathien/soccer
<br>The database is a sqllite database.

The app was based on a sample from Uwe Strerr:
https://www.kaggle.com/nappon/d/hugomathien/soccer/the-fans-stay-loyal-the-players-move-on

The displayed heatmap is renderd via ggplot. The interactivity comes via plotly.

Its possible to filter via Season and League.