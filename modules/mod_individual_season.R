### Tab: Individual Season Data
###
### Primary table = every wrestler-tournament, filtered by the sidebar.
### Select a row -> that wrestler's matches from that tournament.

library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)

individual_season_ui <- function(id, data) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      sliderInput(
        ns("dates"), "Choose a range of years",
        min = min(data$ind_years_formatted$Year),
        max = max(data$ind_years_formatted$Year),
        value = c(1980, max(data$ind_years_formatted$Year)),
        sep = ""
      ),
      jump_year_input(
        ns("jump_year"),
        min(data$ind_years_formatted$Year), max(data$ind_years_formatted$Year)
      ),
      pickerInput(
        ns("team_filter"), "Filter by Team",
        choices = data$team_choices, selected = data$team_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        ns("placement_filter"), "Filter by Placement",
        choices = levels(data$ind_years_formatted$Placement),
        selected = levels(data$ind_years_formatted$Placement),
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        ns("seed_filter"), "Filter by Seed",
        choices = levels(data$ind_years_formatted$Seed),
        selected = levels(data$ind_years_formatted$Seed),
        multiple = TRUE, options = list(`actions-box` = TRUE)
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Individual Tournament Summaries", info_icon(scoring_caveat)),
        season_table_ui(ns("summary")),
        full_screen = TRUE
      ),
      card(
        card_header("Individual Tournament Matches by Selection"),
        match_table_ui(ns("matches")),
        full_screen = TRUE
      )
    )
  )
}

individual_season_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    wire_jump_year(
      input, session, "dates", "jump_year",
      min(data$ind_years_formatted$Year), max(data$ind_years_formatted$Year)
    )

    seasons_reactive <- reactive({
      req(input$dates)

      data$ind_years_formatted %>%
        filter(
          Year >= min(input$dates),
          Year <= max(input$dates),
          Placement %in% input$placement_filter,
          Seed %in% input$seed_filter,
          Team %in% input$team_filter
        ) %>%
        arrange(desc(`Team Points`))
    })

    selected_season <- season_table_server("summary", seasons_reactive)

    matches_reactive <- reactive({
      dat <- selected_season()
      req(nrow(dat) > 0)

      data$matches_master %>%
        filter(
          winner_wrestler_id %in% dat$wrestler_id |
            loser_wrestler_id %in% dat$wrestler_id,
          year %in% dat$Year,
          weight_class %in% dat$Weight
        ) %>%
        arrange(round)
    })

    match_table_server("matches", matches_reactive, show_year = FALSE)
  })
}
