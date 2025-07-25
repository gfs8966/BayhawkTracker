library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyr)

# Replace this with your own sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1NNxDkWscfBXmVXzWXZc3NetHMkP1GZteXrSodDInsBE/edit?resourcekey=&gid=1187787320#gid=1187787320"

# You can read without authentication if sheet is public
gs4_deauth()

# Define UI
ui <- fluidPage(
  tags$div(
    tags$img(src = "logo.jpg", height = "100px"),
    style = "text-align:center; padding-bottom:10px;"
  ),
  
  titlePanel("U11 Bayhawks Coaches Challenge Tracker"),
  tabsetPanel(
    tabPanel("Team Progress", plotOutput("teamtally")),
    tabPanel("Passes Leaderboard", plotOutput("barplot")),
    tabPanel("Pushups Leaderboard", plotOutput("pushups"))
  )
)

team_pushup_target <- 16*500
team_pass_target <- 16*1200
daily_pushups <- ceiling(500/14)
daily_pass <- ceiling(1200/14)

# Define Server
server <- function(input, output, session) {
  
  # Read sheet once at app start (or use reactivePoll for refresh)
  sheet_data <- read_sheet(sheet_url)
  sheet_data <- sheet_data %>% 
    mutate(date_only = as_date(ymd_hms(Timestamp))) %>% 
    rename(Player = `Column 1`, Passes = `How many passes today?`, PushUps = `How many pushups today?`)
  # Barplot output
  output$barplot <- renderPlot({
    # Adjust this depending on your data structure
    ggplot(sheet_data, aes(x = factor(Player), y = Passes)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 1200)
  })
  
  output$pushups <- renderPlot({
    # Adjust this depending on your data structure
    ggplot(sheet_data, aes(x = factor(Player), y = PushUps)) +
      geom_bar(stat = "identity", fill = "purple") +
      theme_minimal() +
      labs(x = "Category", y = "Value") +
      geom_hline(yintercept = 500)
  })
  
  team_totals <- sheet_data %>% group_by(date_only) %>% 
    summarize(PushUps = sum(PushUps), Passes = sum(Passes)) %>% 
    pivot_longer(names_to = "Exercise", values_to = "Reps", -date_only) %>%
    mutate(Reps = cumsum(Reps))
  
  output$teamtally <- renderPlot({
    ggplot(team_totals, aes(date_only, Reps, color = Exercise)) +
      geom_line()+ geom_point() + xlab(element_blank()) +
      theme_minimal() +
      scale_color_manual(values = c("PushUps" = "steelblue", "Passes" = "purple"))
    })
}

# Run the App
shinyApp(ui, server)
