

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
  summarize(Appearances=n())


team_history_ui <- function(id){
  
  ns <- NS(id)

  fluidPage(
  sidebar=sidebar(width=500,
                  
                  conditionalPanel(
    
    "input.nav==`Team History`",
    
    accordion(
      
      accordion_panel(
        
        "Team Filter",
        
        pickerInput(inputId = ns("teamhistory_filter"),
                    label="Choose a Team",
                    choices=team_choices,
                    selected="Penn State",
                    `live-search` = TRUE)
        
        
        )
              )
                  )
  
  )
  )
  
  nav_panel(
    
    "Team History",
    
    page_fillable(
      
      layout_columns(
        
        card(
          
          card_header("Team Summaries"),
          DTOutput("team_summary_table"),
          full_screen = TRUE
          
        )
        
      )
      
    )
    
  )
  
}

team_history_server <- function(id){
  
  moduleServer(id,function(input,output,session) {
    
    team_summary_reactive <- reactive({
      
      team_summaries %>% 
        filter(team %in% input$teamhistory_filter)
      
      
    })
    
    
    output$team_summary_table <- renderDT({
      
      team_summary_reactive()
      
    })
  })
  
}


