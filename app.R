library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(plotly)
# Version: 2026-U13-playoff-push 
# https://gfs8966.shinyapps.io/BayhawkTracker/

color_gradient <- colorRampPalette(c("steelblue", "purple"))(14)
training_range <- data.frame(date_only = seq(from = as.Date("2026-07-01"), to = as.Date("2026-07-31"), by = "day"))

# Replace this with your own sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1iAUHUSUOjGrUQm8nDx8oUYWgX6VnEMLKESKFSiix2Jo/edit?usp=sharing"
gs4_deauth()

team_pushup_target <- 15*30*25
team_pass_target <- 15*30*100
daily_pushups <- ceiling(500/14)
daily_pass <- ceiling(1200/14)

# Define UI
ui <- fluidPage(
  tags$div(
    tags$img(src = "logo.jpg", height = "100px"),
    style = "text-align:center; padding-bottom:10px;"
  ),
  
  titlePanel("U13 Bayhawks Push to Provincials!"),
  tabsetPanel(
    tabPanel("Team Progress", plotlyOutput("passtally"), plotlyOutput("pushuptally")),
    tabPanel("Passes Leaderboard", plotlyOutput("passes")),
    tabPanel("Push Ups Leaderboard", plotlyOutput("pushups")),
    tabPanel("Groundball Leaderboard", plotlyOutput("groundballs")),
    tabPanel("Daily Challenge Leaderboard", plotlyOutput("dailychallenge"))
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Read sheet once at app start (or use reactivePoll for refresh)
  sheet_data <- read_sheet(sheet_url)

  sheet_data <- sheet_data %>% 
    mutate(date_only = as_date(ymd(Date))) %>% 
    rename(Player = Name) %>% 
    mutate(Pushups = suppressWarnings(as.numeric(unlist(Pushups)))) %>% 
    mutate(Passes = suppressWarnings(as.numeric(unlist(Passes)))) %>% 
    mutate(GroundBalls = suppressWarnings(as.numeric(unlist(GroundBalls)))) %>% 
    mutate(Pushups = replace_na(Pushups, 0), Passes = replace_na(Passes, 0)) %>% 
    mutate(DailyWorkout = ifelse(DailyWorkout=="Yes", 1, 0))
  # Barplot output
  output$passes <- renderPlotly({
    # Adjust this depending on your data structure
    p <- ggplot(sheet_data, aes(x = factor(Player), 
                                y = Passes, 
                                fill = as.factor(date_only),
                                text = paste0("Player: ", Player,
                                              "<br>Date: ", date_only,
                                              "<br>Passes: ", Passes))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_gradient, name = "Date") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 1200, lty = 2)
    ggplotly(p, tooltip = "text")
  })
  
  output$pushups <- renderPlotly({
    # Adjust this depending on your data structure
    p2 <- ggplot(sheet_data, aes(x = factor(Player), 
                           y = Pushups, 
                           fill = as.factor(date_only),
                           text = paste0("Player: ", Player,
                                         "<br>Date: ", date_only,
                                         "<br>Push-Ups: ", Pushups))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_gradient, name = "Date") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 500, lty = 2)
    ggplotly(p2, tooltip = "text")
  })
  
  output$groundballs <- renderPlotly({
    # Adjust this depending on your data structure
    p3 <- ggplot(sheet_data, aes(x = factor(Player), 
                                 y = GroundBalls, 
                                 fill = as.factor(date_only),
                                 text = paste0("Player: ", Player,
                                               "<br>Date: ", date_only,
                                               "<br>Ground Balls: ", GroundBalls))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_gradient, name = "Date") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 500, lty = 2)
    ggplotly(p3, tooltip = "text")
  })
  
  output$dailychallenge <- renderPlotly({
    # Adjust this depending on your data structure
    p4 <- ggplot(sheet_data, aes(x = factor(Player), 
                                 y = DailyWorkout, 
                                 fill = as.factor(date_only),
                                 text = paste0("Player: ", Player,
                                               "<br>Date: ", date_only,
                                               "<br>Daily Challenges: ", DailyWorkout))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_gradient, name = "Date") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 30, lty = 2)
    ggplotly(p4, tooltip = "text")
  })
  
  team_totals <- sheet_data %>%  
    group_by(date_only) %>%
    summarize(Pushups = sum(Pushups, na.rm = T), Passes = sum(Passes, na.rm = T)) %>% 
    mutate(Pushups = cumsum(Pushups), Passes = cumsum(Passes)) %>% 
    left_join(training_range, ., by="date_only") %>% 
    mutate(Passes = replace_na(Passes, 0), Pushups = replace_na(Pushups, 0)) 

  output$pushuptally <- renderPlotly({
    p5 <- ggplot(team_totals, aes(date_only, Pushups, text = paste0("Tally: ", Pushups))) +
      geom_bar(stat = "identity", fill = "steelblue") + xlab(element_blank()) +
      theme_minimal() + ggtitle("Push Ups") +
      ylim(c(0, ifelse(team_pushup_target>max(team_totals$Pushups), team_pushup_target, max(team_totals$Pushups)+100))) +
      geom_hline(yintercept = team_pushup_target, lty =2, color = 'grey')
    ggplotly(p5, tooltip = "text")
    })
  
  output$passtally <- renderPlotly({
    p6 <- ggplot(team_totals, aes(date_only, Passes, text = paste0("Tally: ", Passes))) +
      geom_bar(stat = "identity", fill = "steelblue") + xlab(element_blank()) +
      theme_minimal() + ggtitle("Passes") +
      ylim(c(0, ifelse(team_pass_target>max(team_totals$Passes), team_pass_target, max(team_totals$Passes)+100))) +
      geom_hline(yintercept = team_pass_target, lty =2, color = 'grey') 
    ggplotly(p6, tooltip = "text")
  })
}

# Run the App
shinyApp(ui, server)
