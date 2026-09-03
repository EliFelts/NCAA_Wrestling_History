### Tab: Team Scores by Season
###
### Primary table = one row per team-season, filtered by the sidebar. Select a
### row -> that team's individual performances and matches from that tournament.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

team_season_ui <- function(id, data) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      sliderInput(
        ns("years"), "Choose years",
        min = min(data$team_results_annual$year),
        max = max(data$team_results_annual$year),
        value = c(1980, max(data$team_results_annual$year)),
        sep = ""
      ),
      jump_year_input(
        ns("jump_year"),
        min(data$team_results_annual$year), max(data$team_results_annual$year)
      ),
      pickerInput(
        ns("team_filter"), "Filter by Team",
        choices = data$team_choices, selected = data$team_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      checkboxInput(
        ns("count_prelims"), "Count prelim (pigtail) points", value = TRUE
      )
    ),
    layout_columns(
      col_widths = c(6, 6, 6),
      card(
        card_header("Team Scores by Year", info_icon(scoring_caveat)),
        DTOutput(ns("teamscores_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Individual Performances from Selected Team"),
        season_table_ui(ns("seasons")),
        full_screen = TRUE
      ),
      card(
        card_header("Individual Matches from Selected Team"),
        match_table_ui(ns("matches")),
        full_screen = TRUE
      )
    )
  )
}

team_season_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    wire_jump_year(
      input, session, "years", "jump_year",
      min(data$team_results_annual$year), max(data$team_results_annual$year)
    )

    teamscores_reactive <- reactive({
      req(input$years)
      wp <- isTRUE(input$count_prelims)

      data$team_results_annual %>%
        filter(
          year >= min(input$years),
          year <= max(input$years),
          team %in% input$team_filter
        ) %>%
        mutate(
          .score = if (wp) score_wp else score,
          .place = if (wp) place_wp else place
        ) %>%
        arrange(-.score)
    })

    output$teamscores_table <- renderDT({
      dat <- teamscores_reactive() %>%
        transmute(
          Team = team, Year = year, Place = .place, Score = .score,
          Qualifiers = qualifiers, Champs = champs,
          Finalists = finalists, AA = aa,
          `Bonus Points` = bonus_points, Era = era
        )

      dt(
        dat,
        selection = list(mode = "single"),
        caption = scoring_caption(),
        options = list(pageLength = 25)
      )
    })

    selected_team <- reactive({
      teamscores_reactive()[input$teamscores_table_rows_selected, ]
    })

    teamseasons_reactive <- reactive({
      dat <- selected_team()
      req(nrow(dat) > 0)

      data$ind_years_formatted %>%
        filter(team %in% dat$team, year %in% dat$year) %>%
        arrange(Weight)
    })

    season_table_server("seasons", teamseasons_reactive)

    team_matches_reactive <- reactive({
      dat <- selected_team()
      req(nrow(dat) > 0)

      data$matches_master %>%
        filter(
          winner_team %in% dat$team | loser_team %in% dat$team,
          year %in% dat$year
        )
    })

    match_table_server("matches", team_matches_reactive, show_year = TRUE)
  })
}
