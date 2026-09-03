### Shared module: a table of individual tournament seasons.
###
### Same display everywhere -- it's the primary table on the Individual Season
### tab and a drill-down on the Careers, Team, and Bracket tabs. Only the
### reactive that feeds it changes.
###
### season_table_server(id, seasons_data)
###   seasons_data : reactive -> data frame of ind_years_formatted rows
###   returns      : reactive -> the full selected row(s) of seasons_data()
###                  (includes wrestler_id etc. for the next drill-down)

library(shiny)
library(DT)
library(dplyr)

season_table_ui <- function(id) {
  DTOutput(NS(id, "table"))
}

season_table_server <- function(id, seasons_data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      dat <- seasons_data() %>%
        select(
          Name, Team, Weight, Seed, Year, Placement,
          `Team Points`, `Bonus Percent`, place_rank
        )

      # DT sends the ordered Placement factor as plain strings, so sort that
      # column (index 5) off the hidden place_rank column (index 8).
      dt(
        dat,
        selection = "single",
        rownames = FALSE,
        caption = scoring_caption(),
        options = list(
          pageLength = 25,
          columnDefs = list(
            list(visible = FALSE, targets = 8),
            list(orderData = 8, targets = 5)
          )
        )
      )
    })

    reactive({
      seasons_data()[input$table_rows_selected, ]
    })
  })
}
