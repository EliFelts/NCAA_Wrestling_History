### Shared module: a table of matches.
###
### Leaf view used on every tab. The individual-season and bracket drill-downs
### are already scoped to one year so they hide the Year column; the career and
### team drill-downs span years and show it.
###
### match_table_server(id, matches_data, show_year = TRUE)
###   matches_data : reactive -> data frame of matches_master rows

library(shiny)
library(DT)
library(dplyr)

match_table_ui <- function(id) {
  DTOutput(NS(id, "table"))
}

match_table_server <- function(id, matches_data, show_year = TRUE) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderDT({
      dat <- matches_data() %>%
        add_match_display() %>%
        select(
          Year = year, Round = round, Weight = weight_class,
          Winner, Result = result,
          Loser, Score,
          `Termination Time` = termination_time,
          `Team Points Secured` = winner_team_points_secured
        )

      if (!show_year) dat <- select(dat, -Year)

      dt(dat, options = list(pageLength = 25))
    })
  })
}
