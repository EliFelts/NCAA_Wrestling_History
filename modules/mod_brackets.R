### Tab: Weight Class Brackets
###
### Unit of analysis is the (year, weight_class) pair -- for "best bracket ever"
### debates. Primary table ranks brackets; select one -> its field (with
### coming-in and whole-career resumes per wrestler) and its match list.
###
### Metrics come from data/app_tables.rds (bracket_summary_*, bracket_field_*),
### built in data-raw/02_prep_app_data.R. The *_best4 / *_all pair differ only in
### how the career_* columns treat 5-appearance wrestlers -- toggled here.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

brackets_ui <- function(id, data) {
  ns <- NS(id)

  weights <- sort(unique(data$bracket_summary_best4$weight_class))

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      helpText(
        "Cross-era comparisons are biased by bracket and placement rules -- ",
        "the Era column flags which regime each bracket ran under."
      ),
      sliderInput(
        ns("years"), "Choose a range of years",
        min = min(data$bracket_summary_best4$year),
        max = max(data$bracket_summary_best4$year),
        value = c(1996, max(data$bracket_summary_best4$year)),
        sep = ""
      ),
      pickerInput(
        ns("weights"), "Filter by Weight Class",
        choices = weights, selected = weights,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      radioButtons(
        ns("rank_by"), "Rank brackets by",
        choices = c(
          "Returning resume" = "returning_resume",
          "Returning team points" = "returning_points",
          "Career resume (hindsight)" = "career_resume",
          "Career titles won" = "career_title_total"
        ),
        selected = "returning_resume"
      ),
      radioButtons(
        ns("season_cap"), "Career totals count",
        choices = c("Best 4 seasons" = "best4", "All seasons" = "all"),
        selected = "best4"
      ),
      checkboxInput(
        ns("count_prelims"),
        "Count prelim (pigtail) points in the Pts columns", value = TRUE
      )
    ),
    layout_columns(
      col_widths = c(12, 6, 6),
      card(
        card_header(
          "Bracket Rankings",
          info_icon(
            tags$p(tags$b("Returning"), " columns = what the field had banked ",
                   "entering that year (anticipation). ",
                   tags$b("Career"), " columns = how the field turned out ",
                   "over full careers (hindsight)."),
            tags$p(scoring_caveat)
          )
        ),
        DTOutput(ns("brackets_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Field for Selected Bracket"),
        DTOutput(ns("field_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Matches for Selected Bracket"),
        match_table_ui(ns("matches")),
        full_screen = TRUE
      )
    )
  )
}

brackets_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    bracket_summary <- reactive({
      req(input$season_cap)
      data[[paste0("bracket_summary_", input$season_cap)]]
    })

    bracket_field <- reactive({
      req(input$season_cap)
      data[[paste0("bracket_field_", input$season_cap)]]
    })

    brackets_reactive <- reactive({
      req(input$years, input$rank_by)

      dat <- bracket_summary() %>%
        filter(
          year >= min(input$years),
          year <= max(input$years),
          weight_class %in% input$weights
        )

      # The "count prelims" toggle just swaps the points columns for their
      # pigtail-inclusive twins; ranking + display below read the same names.
      if (isTRUE(input$count_prelims)) {
        dat <- dat %>%
          mutate(
            returning_points = returning_points_wp,
            returning_points_per_entrant = returning_points_per_entrant_wp,
            career_points = career_points_wp
          )
      }

      dat %>% arrange(desc(.data[[input$rank_by]]))
    })

    output$brackets_table <- renderDT({
      dat <- brackets_reactive() %>%
        transmute(
          Year = year, Weight = weight_class, Entrants = entrants,
          `Returning Resume` = returning_resume,
          `Returning Pts` = returning_points,
          `Ret. AAs` = returning_aa,
          `Ret. Champs` = returning_champs,
          `Career Resume` = career_resume,
          `Career Pts` = career_points,
          `Career AAs` = career_aa_finishes,
          `Career Champs` = career_champs,
          `Titles Won` = career_title_total,
          `Bonus Rate` = bonus_rate, OT = ot_matches,
          `Avg Margin` = avg_margin,
          Era = era
        )

      dt(
        dat,
        filter = "top",
        selection = "single",
        caption = scoring_caption(),
        options = list(pageLength = 25)
      )
    })

    selected_bracket <- reactive({
      brackets_reactive()[input$brackets_table_rows_selected, ]
    })

    output$field_table <- renderDT({
      dat <- selected_bracket()
      req(nrow(dat) > 0)

      field <- bracket_field() %>%
        filter(year %in% dat$year, weight_class %in% dat$weight_class) %>%
        arrange(place_rank) %>%
        transmute(
          Seed = seed, Wrestler, Team = team,
          Placement = placement, place_rank,
          `Prior Resume` = prior_resume,
          `Prior AAs` = prior_aa,
          `Prior Titles` = prior_titles,
          `Career Resume` = career_resume,
          `Career AAs` = career_aa,
          `Career Titles` = career_titles
        )

      # DT sends the ordered factor as plain strings, so sort the Placement
      # column (index 3) off the hidden place_rank column (index 4).
      dt(
        field,
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          order = list(list(4, "asc")),
          columnDefs = list(
            list(visible = FALSE, targets = 4),
            list(orderData = 4, targets = 3)
          )
        )
      )
    })

    bracket_matches_reactive <- reactive({
      dat <- selected_bracket()
      req(nrow(dat) > 0)

      data$matches_master %>%
        filter(year %in% dat$year, weight_class %in% dat$weight_class) %>%
        arrange(round)
    })

    match_table_server("matches", bracket_matches_reactive, show_year = FALSE)
  })
}
