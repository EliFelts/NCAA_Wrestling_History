### Tab: Match Finder
###
### Unit of analysis is the single match (a row of matches_master). A filterable,
### sortable table for questions like "falls in the championship match?" (Round =
### 1st Place Match, Result = Fall) or "highest-scoring match ever?" (sort by
### Total Pts). Value boxes summarize whatever the current filter selects.
###
### Match scores here are as recorded -- unlike team points, they are not
### reconstructed -- so no scoring caveat applies.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

match_browser_ui <- function(id, data) {
  ns <- NS(id)
  m <- data$matches_master

  round_choices <- levels(m$round)[levels(m$round) %in% unique(m$round)]
  weight_choices <- sort(unique(m$weight_class))
  result_choices <- sort(unique(stats::na.omit(m$result)))

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      helpText(
        "Match scores and bonus values are missing for many older tournaments."
      ),
      sliderInput(
        ns("years"), "Choose a range of years",
        min = min(m$year), max = max(m$year),
        value = c(1980, max(m$year)), sep = ""
      ),
      jump_year_input(ns("jump_year"), min(m$year), max(m$year)),
      pickerInput(
        ns("rounds"), "Round",
        choices = round_choices, selected = round_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        ns("weights"), "Weight Class",
        choices = weight_choices, selected = weight_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        ns("results"), "Result",
        choices = result_choices, selected = result_choices,
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        ns("teams"), "Team (either corner)",
        choices = data$team_choices, selected = data$team_choices,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      checkboxInput(ns("ot_only"), "Overtime only", FALSE)
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      value_box("Matches", textOutput(ns("n_matches"))),
      value_box("Falls", textOutput(ns("n_falls"))),
      value_box("Tech falls", textOutput(ns("n_techs"))),
      value_box("Top score (total pts)", textOutput(ns("top_score")))
    ),
    card(
      card_header("Matches"),
      DTOutput(ns("table")),
      full_screen = TRUE
    )
  )
}

match_browser_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    m_all <- data$matches_master
    round_choices <- levels(m_all$round)[levels(m_all$round) %in% unique(m_all$round)]
    weight_choices <- sort(unique(m_all$weight_class))
    result_choices <- sort(unique(stats::na.omit(m_all$result)))

    wire_jump_year(input, session, "years", "jump_year",
                   min(m_all$year), max(m_all$year))

    filtered <- reactive({
      req(input$years)

      d <- m_all %>%
        filter(year >= min(input$years), year <= max(input$years))

      if (!setequal(input$rounds, round_choices)) {
        d <- filter(d, as.character(round) %in% input$rounds)
      }
      if (!setequal(input$weights, weight_choices)) {
        d <- filter(d, weight_class %in% input$weights)
      }
      if (!setequal(input$results, result_choices)) {
        d <- filter(d, result %in% input$results)
      }
      if (!setequal(input$teams, data$team_choices)) {
        d <- filter(d, winner_team %in% input$teams | loser_team %in% input$teams)
      }
      if (isTRUE(input$ot_only)) {
        d <- filter(d, ot %in% TRUE)
      }
      d
    })

    output$n_matches <- renderText(format(nrow(filtered()), big.mark = ","))
    output$n_falls <- renderText(
      format(sum(filtered()$result == "Fall", na.rm = TRUE), big.mark = ",")
    )
    output$n_techs <- renderText(
      format(sum(filtered()$result == "Technical Fall", na.rm = TRUE), big.mark = ",")
    )
    output$top_score <- renderText({
      v <- suppressWarnings(max(filtered()$total_match_points, na.rm = TRUE))
      if (!is.finite(v)) "--" else format(v)
    })

    output$table <- renderDT({
      dat <- filtered() %>%
        add_match_display() %>%
        mutate(
          OT = ifelse(ot %in% TRUE, "OT", ""),
          round_rank = as.integer(round)
        ) %>%
        arrange(year, round) %>%
        transmute(
          Year = year, Round = round, round_rank, Weight = weight_class,
          Winner, Loser, Score, Result = result,
          `Total Pts` = total_match_points, Margin = margin, OT,
          `Bonus Pts` = bonus_points, `Term. Time` = termination_time
        )

      # Round is an ordered factor; DT sends it as strings, so sort the Round
      # column (index 1) off the hidden round_rank column (index 2).
      dt(
        dat,
        selection = "none",
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          columnDefs = list(
            list(visible = FALSE, targets = 2),
            list(orderData = 2, targets = 1)
          )
        )
      )
    })
  })
}
