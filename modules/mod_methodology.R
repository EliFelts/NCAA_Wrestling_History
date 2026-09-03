### Tab: Scoring Notes  (file kept as mod_methodology.R)
###
### The reconstructed-scoring write-up (articles/methodology.md) plus a compact
### era-boundary table generated from R/fn_eras.R so the two can't drift.
### Takes `data` for signature consistency with the other tabs; unused.

library(shiny)
library(bslib)

methodology_ui <- function(id, data) {
  ns <- NS(id)

  div(
    class = "p-3",
    style = "max-width: 900px; margin: 0 auto;",
    card(
      card_header("Scoring & bracket eras"),
      card_body(tableOutput(ns("eras")))
    ),
    card(
      card_body(includeMarkdown("articles/methodology.md"))
    )
  )
}

methodology_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$eras <- renderTable(
      {
        starts <- c(1928, 1941, 1963, 1972, 1979, 1986, 1996)
        data.frame(
          `Starting` = as.character(starts),
          `Consolation regime` = wrestleback_regime(starts),
          `Places scored` = placement_depth(starts),
          `Era bucket` = tournament_era(starts),
          check.names = FALSE
        )
      },
      striped = TRUE,
      width = "100%",
      digits = 0
    )
  })
}
