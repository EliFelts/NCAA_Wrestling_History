

library(tidyverse)
library(googlesheets4)
library(bslib)
library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(conflicted)
library(bsicons)
library(fontawesome)
library(arrow)
library(hms)
library(forcats)


conflicts_prefer(DT::renderDT,
                 dplyr::filter,
                 dplyr::lag,
                 plotly::layout)

# for now summarize team data to test 
# out getting this UI element to stand alone
# for individual debugging

team_summaries <- team_results_annual %>% 
  group_by(team) %>% 
  summarize(Appearances=n()) %>% 
  ungroup()


team_history_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = ns("teamhistory_filter"),
          label = "Choose a Team",
          choices = unique(team_summaries$team),
          multiple = FALSE,
          options = list(`live-search` = TRUE)
        )
      ),
      mainPanel(
        card(
          card_header("Team Summaries"),
          DTOutput(ns("team_summary_table")),
          full_screen = TRUE
        )
      )
    )
  )

  
}

team_history_server <- function(id){
  
  moduleServer(id,function(input,output,session) {
    
    team_summary_reactive <- reactive({
      
      dat <- team_summaries %>% 
        filter(team %in% input$teamhistory_filter)
      
      
    })
    
    
    output$team_summary_table <- renderDT({
      
      team_summary_reactive()
      
    })
  })
  
}

test_ui <- fluidPage(
  theme = bs_theme(preset = "cerulean"),
  team_history_ui("test_team_history")
)

test_server <- function(input,output,seeion){
  
  team_history_server("test_team_history")
  
}


shinyApp(test_ui,test_server)

