library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(plotly)

color_gradient <- colorRampPalette(c("steelblue", "purple"))(14)
training_range <- data.frame(date_only = seq(from = as.Date("2025-07-25"), to = as.Date("2025-08-07"), by = "day"))

# Replace this with your own sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1NNxDkWscfBXmVXzWXZc3NetHMkP1GZteXrSodDInsBE/edit?usp=sharing"
gs4_deauth()

team_pushup_target <- 16*500
team_pass_target <- 16*1200
daily_pushups <- ceiling(500/14)
daily_pass <- ceiling(1200/14)

# Define UI
ui <- fluidPage(
  tags$div(
    tags$img(src = "logo.jpg", height = "100px"),
    style = "text-align:center; padding-bottom:10px;"
  ),
  
  titlePanel("U11 Bayhawks Coaches Challenge Tracker"),
  tabsetPanel(
    tabPanel("Team Progress", plotlyOutput("passtally"), plotlyOutput("pushuptally")),
    tabPanel("Passes Leaderboard", plotlyOutput("passes")),
    tabPanel("Push Ups Leaderboard", plotlyOutput("pushups"))
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Read sheet once at app start (or use reactivePoll for refresh)
  sheet_data <- read_sheet(sheet_url)

  sheet_data <- sheet_data %>% 
    mutate(date_only = as_date(ymd_hms(Timestamp))) %>% 
    rename(Player = `Column 1`, Passes = `How many passes today?`, PushUps = `How many pushups today?`) %>% 
    mutate(PushUps = suppressWarnings(as.numeric(unlist(PushUps)))) %>% 
    mutate(Passes = suppressWarnings(as.numeric(unlist(Passes)))) %>% 
    mutate(PushUps = replace_na(PushUps, 0), Passes = replace_na(Passes, 0))
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
                           y = PushUps, 
                           fill = as.factor(date_only),
                           text = paste0("Player: ", Player,
                                         "<br>Date: ", date_only,
                                         "<br>Push-Ups: ", PushUps))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = color_gradient, name = "Date") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 500, lty = 2)
    ggplotly(p2, tooltip = "text")
  })
  
  team_totals <- sheet_data %>%  
    group_by(date_only) %>%
    summarize(PushUps = sum(PushUps, na.rm = T), Passes = sum(Passes, na.rm = T)) %>% 
    mutate(PushUps = cumsum(PushUps), Passes = cumsum(Passes)) %>% 
    left_join(training_range, ., by="date_only") %>% 
    mutate(Passes = replace_na(Passes, 0), PushUps = replace_na(PushUps, 0)) 

  output$pushuptally <- renderPlotly({
    p3 <- ggplot(team_totals, aes(date_only, PushUps, text = paste0("Tally: ", PushUps))) +
      geom_bar(stat = "identity", fill = "steelblue") + xlab(element_blank()) +
      theme_minimal() + ggtitle("Push Ups") +
      ylim(c(0, ifelse(team_pushup_target>max(team_totals$PushUps), team_pushup_target, max(team_totals$PushUps)+100))) +
      geom_hline(yintercept = team_pushup_target, lty =2, color = 'grey')
    ggplotly(p3, tooltip = "text")
    })
  
  output$passtally <- renderPlotly({
    p4 <- ggplot(team_totals, aes(date_only, Passes, text = paste0("Tally: ", Passes))) +
      geom_bar(stat = "identity", fill = "steelblue") + xlab(element_blank()) +
      theme_minimal() + ggtitle("Passes") +
      ylim(c(0, ifelse(team_pass_target>max(team_totals$Passes), team_pass_target, max(team_totals$Passes)+100))) +
      geom_hline(yintercept = team_pass_target, lty =2, color = 'grey') 
    ggplotly(p4, tooltip = "text")
  })
}

# Run the App
shinyApp(ui, server)
