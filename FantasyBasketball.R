#Fantasy Basketball Simulator
rsconnect::setAccountInfo(name='billy-stewart',
                          token='8C0BBB0FBF50C85E7B241785C3CA2581',
                          secret='ouoWuKGCudLsFvbXG9nbd1cm03H/lYqfTEM/5J40')

#Install Packages
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinycssloaders)

#Load the Data
game_dfs <- readRDS("~\\Stats\\Sports Stats\\Fantasy Basketball App\\Fantasy_Stats.rdata")
players <- list(unique(game_dfs$Player))

ui <- fluidPage(
  #Create Title
  titlePanel("Fantasy Basketball Comparison"),
  
  #Create place to input user players
  fluidRow(
    column(width = 3,
      "Your Team",
      selectInput("upg", "PG", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("usg", "SG", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("usf", "SF", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("upf", "PF", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("uc", "C", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("ug", "G", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("uf", "F", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("uutl1", "UTL", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("uutl2", "UTL", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("uutl3", "UTL", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("ube1", "BE", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("ube2", "BE", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("ube3", "BE", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      actionButton("action1", "Submit", class = "btn-primary")
    ),
    
    column(width = 6),
      
    column(width = 3,
      "Opponent's Team",
      selectInput("opg", "PG", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("osg", "SG", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("osf", "SF", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("opf", "PF", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("oc", "C", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("og", "G", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("of", "F", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("outl1", "UTL", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("outl2", "UTL", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("outl3", "UTL", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("obe1", "BE", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("obe2", "BE", choices = sort(unique(game_dfs$players, decreasing = TRUE))),
      selectInput("obe3", "BE", choices = sort(unique(game_dfs$players, decreasing = TRUE)))
    )
  )
)

server <- function(input, output){
  output$overallplot <- eventReactive("action1", {
    
  })
  
}

shinyApp(ui, server)

rsconnect::deployApp('~\\Stats\\Sports Stats\\Fantasy Basketball App')