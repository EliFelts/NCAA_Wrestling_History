### Tab: Team History (all-time)  -- NOT WIRED IN YET
###
### A team's whole-history summary, distinct from "Team Scores by Season". Kept
### in the module contract so it's a one-liner to add to app.R once the display
### is fleshed out:
###   nav_panel("Team History", team_history_ui("team_history", app_data))
###   team_history_server("team_history", app_data)

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

team_history_ui <- function(id, data) {
  ns <- NS(id)

  team_summaries <- data$team_results_annual %>%
    group_by(team) %>%
    summarize(
      Appearances = n(),
      `Individual Titles` = sum(champs),
      .groups = "drop"
    )

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      pickerInput(
        ns("team"), "Choose a Team",
        choices = sort(unique(team_summaries$team)),
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      )
    ),
    card(
      card_header("Team Summaries"),
      DTOutput(ns("table")),
      full_screen = TRUE
    )
  )
}

team_history_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    team_summaries <- data$team_results_annual %>%
      group_by(team) %>%
      summarize(
        Appearances = n(),
        `Individual Titles` = sum(champs),
        .groups = "drop"
      )

    output$table <- renderDT({
      req(input$team)
      dt(filter(team_summaries, team %in% input$team))
    })
  })
}
