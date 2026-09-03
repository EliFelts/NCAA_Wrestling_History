### Tab: Seed Performance
###
### Unit of analysis is the seed. Primary table = one row per seed (1..33 plus
### "Unseeded"), aggregated over the seasons the sidebar selects, for questions
### like "how often does a 1-seed miss AA?" or "how many unseeded wrestlers have
### made the finals?". Select a seed -> its individual seasons -> that season's
### matches.
###
### Aggregation is done live off ind_years_formatted (~24k rows) because the
### year/weight window is applied before grouping -- no prepared table.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

seed_performance_ui <- function(id, data) {
  ns <- NS(id)
  iy <- data$ind_years_formatted
  weight_choices <- sort(unique(iy$weight_class))

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      info_icon(
        paste(
          "Seeding depth has grown over time -- only a handful of wrestlers",
          "were seeded in the early tournaments, expanding to 8, then 12, 16,",
          "and 33 (with pigtails). High seed numbers and the 'Unseeded' pool",
          "mean very different things across eras."
        ),
        title = "Reading seeds across eras"
      ),
      sliderInput(
        ns("dates"), "Choose a range of years",
        min = min(iy$Year), max = max(iy$Year),
        value = c(1980, max(iy$Year)), sep = ""
      ),
      jump_year_input(ns("jump_year"), min(iy$Year), max(iy$Year)),
      pickerInput(
        ns("weights"), "Weight Class",
        choices = weight_choices, selected = weight_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      radioButtons(
        ns("rate_basis"), "Show rate columns as",
        choices = c("Percent" = "pct", "Count" = "count"),
        selected = "pct"
      )
    ),
    layout_columns(
      col_widths = c(12, 6, 6),
      card(
        card_header("Performance by Seed"),
        DTOutput(ns("seed_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Seasons for Selected Seed", info_icon(scoring_caveat)),
        season_table_ui(ns("seasons")),
        full_screen = TRUE
      ),
      card(
        card_header("Matches for Selected Season"),
        match_table_ui(ns("matches")),
        full_screen = TRUE
      )
    )
  )
}

seed_performance_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    weight_choices <- sort(unique(data$ind_years_formatted$weight_class))

    wire_jump_year(
      input, session, "dates", "jump_year",
      min(data$ind_years_formatted$Year), max(data$ind_years_formatted$Year)
    )

    windowed <- reactive({
      req(input$dates)

      d <- data$ind_years_formatted %>%
        filter(Year >= min(input$dates), Year <= max(input$dates))

      if (!setequal(input$weights, weight_choices)) {
        d <- filter(d, weight_class %in% input$weights)
      }
      d
    })

    seed_summary <- reactive({
      as_pct <- identical(input$rate_basis, "pct")

      windowed() %>%
        group_by(Seed) %>%
        summarize(
          seed_sort = dplyr::first(coalesce(seed, 999L)),
          N = n(),
          Champs = sum(place_rank == 1),
          Finalists = sum(place_rank <= 2),
          AAs = sum(place_rank <= 8),
          `Missed AA` = sum(place_rank > 8),
          .avg_finish = ifelse(
            AAs > 0, round(mean(place_rank[place_rank <= 8]), 1), NA_real_
          ),
          .avg_pts = round(mean(`Team Points`, na.rm = TRUE), 1),
          .p_title = Champs / N,
          .p_finals = Finalists / N,
          .p_aa = AAs / N,
          .groups = "drop"
        ) %>%
        arrange(seed_sort) %>%
        mutate(
          `Title %` = if (as_pct) round(100 * .p_title) else Champs,
          `Finals %` = if (as_pct) round(100 * .p_finals) else Finalists,
          `AA %` = if (as_pct) round(100 * .p_aa) else AAs,
          `Avg AA Finish` = .avg_finish,
          `Avg Team Pts` = .avg_pts
        ) %>%
        select(
          Seed, seed_sort, N, `Title %`, `Finals %`, `AA %`,
          Champs, Finalists, AAs, `Missed AA`,
          `Avg AA Finish`, `Avg Team Pts`
        )
    })

    output$seed_table <- renderDT({
      dat <- seed_summary()

      # Seed is a factor; DT sends it as strings, so sort that column (index 0)
      # off the hidden numeric seed_sort column (index 1).
      dt(
        dat,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 34,
          dom = "t",
          order = list(list(1, "asc")),
          columnDefs = list(
            list(visible = FALSE, targets = 1),
            list(orderData = 1, targets = 0)
          )
        )
      )
    })

    selected_seed <- reactive({
      seed_summary()[input$seed_table_rows_selected, ]
    })

    seasons_reactive <- reactive({
      s <- selected_seed()
      req(nrow(s) > 0)

      windowed() %>%
        filter(Seed %in% s$Seed) %>%
        arrange(place_rank, desc(`Team Points`))
    })

    selected_season <- season_table_server("seasons", seasons_reactive)

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

    match_table_server("matches", matches_reactive, show_year = TRUE)
  })
}
