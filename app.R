#Fantasy Basketball Simulator

#Install Packages
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(shiny)
library(sn)
library(shinycssloaders)

#Load the Data
fantasy_stats <- readRDS("Fantasy_Stats.rdata")
fantasy_schedule <- readRDS("Fantasy_Schedule.rdata")

ui <- fluidPage(
    #Create Title
    titlePanel("Fantasy Basketball Comparison"),
    
    #Create place to input user players
    fluidRow(
        column(width = 3,
               "Your Team",
               selectInput("upg", "PG", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("usg", "SG", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("usf", "SF", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("upf", "PF", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("uc", "C", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("ug", "G", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("uf", "F", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("uutl1", "UTL", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("uutl2", "UTL", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("uutl3", "UTL", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("ube1", "BE", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("ube2", "BE", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("ube3", "BE", choices = unique(sort(as.character(fantasy_stats$Player)))),
               actionButton("action1", "Submit", class = "btn-primary")
        ),
        
        #Create area for graphs with spinners
        column(width = 6,
               selectInput("week", "WEEK NUMBER", choices = 1:21),
               verbatimTextOutput("wins"),
               verbatimTextOutput("ties"),
               verbatimTextOutput("loss"),
               plotOutput("fgperc")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("ftperc")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("threept")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("rebounds")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("assists")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("steals")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("blocks")  %>% withSpinner(color="#0dc5c1"),
               plotOutput("points")  %>% withSpinner(color="#0dc5c1")),
        
        #Create column to input opponent players  
        column(width = 3,
               "Opponent's Team",
               selectInput("opg", "PG", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("osg", "SG", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("osf", "SF", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("opf", "PF", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("oc", "C", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("og", "G", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("of", "F", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("outl1", "UTL", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("outl2", "UTL", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("outl3", "UTL", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("obe1", "BE", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("obe2", "BE", choices = unique(sort(as.character(fantasy_stats$Player)))),
               selectInput("obe3", "BE", choices = unique(sort(as.character(fantasy_stats$Player))))
        )
    )
)


server <- function(input, output){
    #Pull user players' data
    user_data <- eventReactive(input$action1, {
        temp1 <- fantasy_stats %>% filter(Player == input$upg | Player == input$usg | Player == input$usf | Player == input$upf |
                                     Player == input$uc | Player == input$ug | Player == input$uf | Player == input$uutl1 |
                                     Player == input$uutl2 | Player == input$uutl3 | Player == input$ube1 | Player == input$ube2 |
                                     Player == input$ube3)
        return(temp1)
    })
    
    #Pull opponent players' data
    opp_data <- eventReactive(input$action1, {
        temp1 <- fantasy_stats %>% filter(Player == input$opg | Player == input$osg | Player == input$osf | Player == input$opf |
                                     Player == input$oc | Player == input$og | Player == input$of | Player == input$outl1 |
                                     Player == input$outl2 | Player == input$outl3 | Player == input$obe1 | Player == input$obe2 |
                                     Player == input$obe3)
        return(temp1)
    })
    
    #Do 100 simulations of a week for user players
    user_sims <- eventReactive(input$action1, {
        temp1 <- aggregate(user_data()[,1:15], list(user_data()$Player), mean)
        temp2 <- aggregate(user_data()[,1:15], list(user_data()$Player), sd)
        temp3 <- merge(temp1, temp2, by = "Group.1")
        
        temp4 <- merge(temp3, user_data()[,16:18], by.x = "Group.1", by.y = "Player")
        temp3 <- unique(temp4)
        
        test1 <- NULL
        for(k in 1:1000) {
            temp4 <- temp6 <- NULL
            for(i in 1:length(temp3$Group.1)) {
                temp5 <- NULL
                num_games <- fantasy_schedule[[paste(temp3$Team[i])]][as.numeric(input$week)]
                for(j in c(4,5,10:15)) {
                    if(j == 4 | j == 10) {
                        temp4 <- mean(rnorm(num_games, mean = temp3[i,j], sd = temp3[i,j+15]))
                    } else if (j == 15) {
                        temp4 <- sum(round(rnorm(num_games, mean = temp3[i,j], sd = temp3[i,j+15])))
                    } else {
                        temp4 <- sum(round(rsn(num_games, xi = temp3[i,j], omega = temp3[i,j+15], tau = 1, alpha = 3)))
                    }
                    if (temp4 < 0) {temp4 = 0}
                    temp5 <- cbind(temp5, temp4)
                }
                temp5 <- cbind(temp3[i,1], temp5)
                temp6 <- rbind(temp6, temp5)
            }
            temp7 <- as.data.frame(temp6)
            names(temp7) <- c("Player", "FG%", "3Pt", "FT%", "Rebound", "Assists", "Steals", "Blocks", "Points")
            temp7[,2:9] <- apply(temp7[,2:9], 2, as.numeric)
            test1[[k]] <- temp7
        }
        return(test1)
    })
    
    #Do 100 simulations of week for opponents
    opp_sims <- eventReactive(input$action1, {
        temp1 <- aggregate(opp_data()[,1:15], list(opp_data()$Player), mean)
        temp2 <- aggregate(opp_data()[,1:15], list(opp_data()$Player), sd)
        temp3 <- merge(temp1, temp2, by = "Group.1")
        
        temp4 <- merge(temp3, opp_data()[16:18], by.x = "Group.1", by.y = "Player")
        temp3 <- unique(temp4)
        
        test1 <- NULL
        for(k in 1:1000) {
            temp4 <- temp6 <- NULL
            for(i in 1:length(temp3$Group.1)) {
                temp5 <- NULL
                num_games <- fantasy_schedule[[paste(temp3$Team[i])]][as.numeric(input$week)]
                for(j in c(4,5,10:15)) {
                    if(j == 4 | j == 10) {
                        temp4 <- mean(rnorm(num_games, mean = temp3[i,j], sd = temp3[i,j+15]))
                    } else if (j == 15) {
                        temp4 <- sum(round(rnorm(num_games, mean = temp3[i,j], sd = temp3[i,j+15])))
                    } else {
                        temp4 <- sum(round(rsn(num_games, xi = temp3[i,j], omega = temp3[i,j+15], tau = 1, alpha = 3)))
                    }
                    if (temp4 < 0) {temp4 = 0}
                    temp5 <- cbind(temp5, temp4)
                }
                temp5 <- cbind(temp3[i,1], temp5)
                temp6 <- rbind(temp6, temp5)
            }
            temp7 <- as.data.frame(temp6)
            names(temp7) <- c("Player", "FG%", "3Pt", "FT%", "Rebound", "Assists", "Steals", "Blocks", "Points")
            temp7[,2:9] <- apply(temp7[,2:9], 2, as.numeric)
            test1[[k]] <- temp7
        }
        return(test1)
    })
    
    #Aggregate total weekly values for user
    user_tots <- eventReactive(input$action1, {
        temp1 <- temp2 <- temp3 <- NULL
        for(i in 1:100){
            for(j in 2:9){
                if(j == 2 | j == 4){
                    temp1 <- mean(user_sims()[[i]][,j])
                } else {
                    temp1 <- sum(user_sims()[[i]][,j])
                }
                temp2 <- cbind(temp2, temp1)
            }
            temp3 <- rbind(temp3, temp2)
            temp2 <- NULL
            temp3 <- as.data.frame(temp3)
        }
        names(temp3) <- c("FG%", "3Pt", "FT%", "Rebounds", "Assists", "Steals", "Blocks", "Points")
        return(temp3)
    })
    
    #Aggregate total weekly values for opponent
    opp_tots <- eventReactive(input$action1, {
        temp1 <- temp2 <- temp3 <- NULL
        for(i in 1:100){
            for(j in 2:9){
                if(j == 2 | j == 4){
                    temp1 <- mean(opp_sims()[[i]][,j])
                } else {
                    temp1 <- sum(opp_sims()[[i]][,j])
                }
                temp2 <- cbind(temp2, temp1)
            }
            temp3 <- rbind(temp3, temp2)
            temp2 <- NULL
            temp3 <- as.data.frame(temp3)
        }
        names(temp3) <- c("FG%", "3Pt", "FT%", "Rebounds", "Assists", "Steals", "Blocks", "Points")
        return(temp3)
    })
    
    #Create table to show if they won or lost or tied for that week
    win_table <- eventReactive(input$action1, {
        temp1 <- NULL
        for(i in 1:100){
            wins <- NULL
            for(j in 1:8) {
                if(user_tots()[i,j] > opp_tots()[i,j]){
                    wins[j] <- "W"
                } else if(user_tots()[i,j] == opp_tots()[i,j]){
                    wins[j] <- "T"
                } else {
                    wins[j] <- "L"
                }
            }
            if(sum(wins == "W") > sum(wins == "L")){
                temp1[i] <- "W"
            } else if(sum(wins == "W") < sum(wins == "L")){
                temp1[i] <- "L"
            } else {
                temp1[i] <- "T"
            }
        }
        return(temp1)
    })
    
    #Calculate chance of winning, tying, and losing
    output$wins <- eventReactive(input$action1, {
        w <- mean(win_table() == "W")
        return(paste0("Your win percentage is ", w))
    })
    
    output$ties <- eventReactive(input$action1, {
        t <- mean(win_table() == "T")
        return(paste0("Your tie percentage is ", t))
    })
    
    output$loss <- eventReactive(input$action1, {
        l <- mean(win_table() == "L")
        return(paste0("Your loss percentage is ", l))
    })
    
    #Make graphs for different stats based on different columns of data
    output$fgperc <- renderPlot({
        ggplot() +
            geom_density(data = user_tots(), mapping = aes(x = `FG%`), col = "blue", size = 2) +
            geom_density(data = opp_tots(), mapping = aes(x = `FG%`), col = "red", size = 2) +
            ggtitle("FIELD GOAL PERCENTAGE") +
            xlab("FG%") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    })
    
    output$ftperc <- renderPlot({
        ggplot() +
            geom_density(data = user_tots(), mapping = aes(x = `FT%`), col = "blue", size = 2) +
            geom_density(data = opp_tots(), mapping = aes(x = `FT%`), col = "red", size = 2) +
            ggtitle("FREE THROW PERCENTAGE") +
            xlab("FT%") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    })
    
    output$threept <- renderPlot({
        ggplot() +
            geom_histogram(data = user_tots(), mapping = aes(x = `3Pt`), col = "blue", fill = "blue", size = 2, alpha = .4) +
            geom_histogram(data = opp_tots(), mapping = aes(x = `3Pt`), col = "red", fill = "red", size = 2, alpha = .4) +
            ggtitle("3 POINTERS") +
            xlab("3Pts") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    })
    
    output$rebounds <- renderPlot({
        ggplot() +
            geom_density(data = user_tots(), mapping = aes(x = Rebounds), col = "blue", size = 2) +
            geom_density(data = opp_tots(), mapping = aes(x = Rebounds), col = "red", size = 2) +
            ggtitle("REBOUNDS") +
            xlab("Rebounds") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    }) 
    
    output$assists <- renderPlot({
        ggplot() +
            geom_histogram(data = user_tots(), mapping = aes(x = Rebounds), col = "blue", fill = "blue", size = 2, alpha = .4) +
            geom_histogram(data = opp_tots(), mapping = aes(x = Rebounds), col = "red", fill = "red", size = 2, alpha = .4) +
            ggtitle("ASSISTS") +
            xlab("Assists") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    }) 
    
    output$steals <- renderPlot({
        ggplot() +
            geom_histogram(data = user_tots(), mapping = aes(x = Steals), col = "blue", fill = "blue", size = 2, alpha = .4) +
            geom_histogram(data = opp_tots(), mapping = aes(x = Steals), col = "red", fill = "red", size = 2, alpha = .4) +
            ggtitle("STEALS") +
            xlab("Steals") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    }) 
    
    output$blocks <- renderPlot({
        ggplot() +
            geom_histogram(data = user_tots(), mapping = aes(x = Blocks), col = "blue", fill = "blue", size = 2, alpha = .4) +
            geom_histogram(data = opp_tots(), mapping = aes(x = Blocks), col = "red", fill = "red", size = 2, alpha = .4) +
            ggtitle("BLOCKS") +
            xlab("Blocks") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    }) 
    
    output$points <- renderPlot({
        ggplot() +
            geom_density(data = user_tots(), mapping = aes(x = Points), col = "blue", size = 2) +
            geom_density(data = opp_tots(), mapping = aes(x = Points), col = "red", size = 2) +
            ggtitle("POINTS") +
            xlab("Points") +
            ylab("Density") +
            theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
    })
}


shinyApp(ui, server)
