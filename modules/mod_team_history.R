### Tab: Team History (all-time)
###
### Unit of analysis is the team. Primary table = one row per team over the
### selected year range: tournament finishes (titles / runner-up / top 4 / top
### 10 / best) off official_place, and individual hardware (champions,
### All-Americans, and the DISTINCT wrestler counts behind them) off
### ind_years_formatted. Select a team -> its season-by-season results and its
### champion / All-American roster; select a season -> that year's individual
### performances.
###
### Aggregated live off team_results_annual (~6.7k rows) and
### ind_years_formatted (~24k) -- the year-range filter applies before
### grouping, so there is no prepared table.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

# min() that yields NA (not Inf + a warning) when nothing qualifies.
.safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) min(x) else NA_integer_
}

.place_label <- c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th")

team_history_ui <- function(id, data) {
  ns <- NS(id)
  yr_rng <- range(data$team_results_annual$year)

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      helpText(
        "All-time team summaries. Tournament finishes use official team ",
        "place; the individual counts use reconstructed placements."
      ),
      sliderInput(
        ns("years"), "Year range",
        min = yr_rng[1], max = yr_rng[2], value = yr_rng, sep = ""
      ),
      numericInput(ns("min_app"), "Minimum appearances", value = 1, min = 1, step = 1)
    ),
    layout_columns(
      col_widths = c(12, 6, 6, 12),
      card(
        card_header("All-Time Team Ledger", info_icon(scoring_caveat)),
        DTOutput(ns("ledger")),
        full_screen = TRUE
      ),
      card(
        card_header("Season by Season -- Selected Team"),
        DTOutput(ns("seasons")),
        full_screen = TRUE
      ),
      card(
        card_header("Champions & All-Americans -- Selected Team"),
        DTOutput(ns("people")),
        full_screen = TRUE
      ),
      card(
        card_header("Individual Performances -- Selected Season"),
        season_table_ui(ns("ind")),
        full_screen = TRUE
      )
    )
  )
}

team_history_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    ledger <- reactive({
      req(input$years)
      y0 <- min(input$years); y1 <- max(input$years)
      min_app <- input$min_app
      if (is.null(min_app) || is.na(min_app)) min_app <- 1

      team_fin <- data$team_results_annual %>%
        filter(year >= y0, year <= y1) %>%
        group_by(team) %>%
        summarize(
          Appearances = n(),
          Titles = sum(official_place == 1, na.rm = TRUE),
          `Runner-Up` = sum(official_place == 2, na.rm = TRUE),
          `Top 4` = sum(official_place <= 4, na.rm = TRUE),
          `Top 10` = sum(official_place <= 10, na.rm = TRUE),
          best_place = .safe_min(official_place),
          `Total Off. Pts` = round(sum(official_score, na.rm = TRUE), 1),
          .groups = "drop"
        )

      team_ind <- data$ind_years_formatted %>%
        filter(Year >= y0, Year <= y1) %>%
        group_by(team = Team) %>%
        summarize(
          `Ind. Titles` = sum(place_rank == 1),
          `Distinct Champs` = n_distinct(wrestler_id[place_rank == 1]),
          `Finals` = sum(place_rank <= 2),
          `AA Finishes` = sum(place_rank <= 8),
          `Distinct AAs` = n_distinct(wrestler_id[place_rank <= 8]),
          .groups = "drop"
        )

      team_fin %>%
        left_join(team_ind, by = "team") %>%
        mutate(across(
          c(`Ind. Titles`, `Distinct Champs`, Finals, `AA Finishes`, `Distinct AAs`),
          ~ tidyr::replace_na(.x, 0L)
        )) %>%
        filter(Appearances >= min_app) %>%
        transmute(
          Team = team, Appearances,
          Titles, `Runner-Up`, `Top 4`, `Top 10`,
          `Best` = best_place,
          `Ind. Titles`, `Distinct Champs`, Finals,
          `AA Finishes`, `Distinct AAs`,
          `Total Off. Pts`
        ) %>%
        arrange(desc(Titles), desc(`Top 4`), desc(`AA Finishes`))
    })

    output$ledger <- renderDT({
      # Best (col 6) is a place integer; show the label but sort ascending
      # (1st is best) -- it is already the raw integer so a plain asc sort works.
      dt(
        ledger(),
        selection = "single",
        rownames = FALSE,
        filter = "top",
        caption = scoring_caption(),
        options = list(pageLength = 25, order = list(list(2, "desc")))
      )
    })

    selected_team <- reactive({
      ledger()[input$ledger_rows_selected, ]
    })

    team_seasons <- reactive({
      st <- selected_team()
      req(nrow(st) > 0)
      data$team_results_annual %>%
        filter(
          team == st$Team,
          year >= min(input$years), year <= max(input$years)
        ) %>%
        arrange(year)
    })

    output$seasons <- renderDT({
      dat <- team_seasons() %>%
        transmute(
          Year = year, `Off. Place` = official_place,
          `Off. Score` = official_score, `Recon. Pts` = score,
          Qual = qualifiers, Champs = champs, Finalists = finalists,
          AA = aa, `Bonus Pts` = bonus_points, Era = era
        )
      dt(
        dat,
        selection = "single",
        rownames = FALSE,
        caption = scoring_caption(),
        options = list(pageLength = 25, order = list(list(0, "asc")))
      )
    })

    output$people <- renderDT({
      st <- selected_team()
      req(nrow(st) > 0)

      dat <- data$ind_years_formatted %>%
        filter(
          Team == st$Team, place_rank <= 8,
          Year >= min(input$years), Year <= max(input$years)
        ) %>%
        group_by(wrestler_id, Wrestler = Name) %>%
        summarize(
          Titles = sum(place_rank == 1),
          `AA Finishes` = n(),
          best_rank = min(place_rank),
          Weights = paste(sort(unique(Weight)), collapse = ", "),
          Years = paste(sort(unique(Year)), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(best_rank, desc(Titles), desc(`AA Finishes`), Years) %>%
        transmute(
          Wrestler, Titles, `AA Finishes`,
          `Best Finish` = .place_label[best_rank], best_rank,
          Weights, Years
        )

      # Best Finish (col 3) is a label; sort it off the hidden best_rank (col 4).
      dt(
        dat,
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          order = list(list(4, "asc"), list(1, "desc")),
          columnDefs = list(
            list(visible = FALSE, targets = 4),
            list(orderData = 4, targets = 3)
          )
        )
      )
    })

    selected_season <- reactive({
      team_seasons()[input$seasons_rows_selected, ]
    })

    ind_seasons <- reactive({
      ss <- selected_season()
      req(nrow(ss) > 0)
      data$ind_years_formatted %>%
        filter(team == ss$team, year == ss$year) %>%
        arrange(Weight)
    })

    season_table_server("ind", ind_seasons)

    reactive(selected_team())
  })
}
