### Tab: Individual Career Data
###
### Primary table = one row per wrestler (career roll-up), filtered by the
### sidebar. Select a row -> that wrestler's individual seasons and full-career
### match list.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)
library(stringr)

careers_ui <- function(id, data) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      sliderInput(
        ns("dates"), "Choose a range of years",
        min = min(data$ind_years_formatted$Year),
        max = max(data$careers_summary$career_end),
        value = c(1980, max(data$careers_summary$career_end)),
        sep = ""
      ),
      pickerInput(
        ns("team_filter"), "Filter by Team",
        choices = data$team_choices, selected = data$team_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
    ),
    layout_columns(
      col_widths = c(6, 6, 6),
      card(
        card_header("Career Summaries", info_icon(scoring_caveat)),
        DTOutput(ns("careers_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Individual Seasons for Selected Wrestler"),
        season_table_ui(ns("seasons")),
        full_screen = TRUE
      ),
      card(
        card_header("Matches for Selected Wrestler"),
        match_table_ui(ns("matches")),
        full_screen = TRUE
      )
    )
  )
}

careers_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    careers_reactive <- reactive({
      req(input$dates)

      teams_selected <- input$team_filter
      team_pattern <- paste0(
        "(^|,\\s*)(",
        paste0(teams_selected, collapse = "|"),
        ")(?=,|$)"
      )

      data$careers_formatted %>%
        filter(
          career_start >= min(input$dates),
          career_end <= max(input$dates),
          str_detect(`Team(s)`, regex(team_pattern))
        ) %>%
        arrange(desc(`Team Points`))
    })

    output$careers_table <- renderDT({
      dat <- careers_reactive() %>%
        select(-c(
          wrestler_id, career_start, career_end,
          Falls, `Total Falls Time`, `Bonus Wins`
        ))

      dt(
        dat,
        filter = "top",
        selection = "single",
        caption = scoring_caption(),
        options = list(pageLength = 25)
      )
    })

    selected_career <- reactive({
      careers_reactive()[input$careers_table_rows_selected, ]
    })

    seasons_reactive <- reactive({
      dat <- selected_career()
      req(nrow(dat) > 0)

      data$ind_years_formatted %>%
        filter(wrestler_id %in% dat$wrestler_id) %>%
        arrange(Year)
    })

    season_table_server("seasons", seasons_reactive)

    career_matches_reactive <- reactive({
      dat <- selected_career()
      req(nrow(dat) > 0)

      data$matches_master %>%
        filter(
          winner_wrestler_id %in% dat$wrestler_id |
            loser_wrestler_id %in% dat$wrestler_id
        ) %>%
        arrange(year, round)
    })

    match_table_server("matches", career_matches_reactive, show_year = TRUE)
  })
}
