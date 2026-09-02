### NCAA Wrestling Tournament History -- Shiny app
###
### app.R is the shell only: load data, source the pieces, assemble the navbar.
### Each tab lives in modules/mod_<tab>.R and is handed the prepared data.
###
### Data prep (run from the project root when data/ is stale):
###   Rscript data-raw/01_pull_sheets.R     Google Sheets -> data/{matches,seeds,wrestlers}.rds
###   Rscript data-raw/02_prep_app_data.R   data/*.rds     -> data/app_tables.rds

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(plotly)
library(tidyverse)
library(hms)
library(forcats)
library(conflicted)
library(bsicons)
library(fontawesome)

conflicts_prefer(
  DT::renderDT,
  dplyr::filter,
  dplyr::lag,
  plotly::layout
)

if (!file.exists("data/app_tables.rds")) {
  stop("data/app_tables.rds not found -- run: Rscript data-raw/02_prep_app_data.R")
}
app_data <- readRDS("data/app_tables.rds")

# Helpers then modules. R/ auto-sourcing by Shiny is disabled
# (R/_disable_autoload.R); load explicitly so order is predictable.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
for (f in c(
  "modules/mod_season_table.R",
  "modules/mod_match_table.R",
  "modules/mod_individual_season.R",
  "modules/mod_careers.R",
  "modules/mod_team_season.R",
  "modules/mod_brackets.R",
  "modules/mod_match_browser.R",
  "modules/mod_methodology.R"
)) source(f)

ui <- page_navbar(
  title = "NCAA Wrestling Tournament Results",
  theme = bs_theme(preset = "cerulean"),
  nav_panel("Individual Season Data", individual_season_ui("individual", app_data)),
  nav_panel("Individual Career Data", careers_ui("careers", app_data)),
  nav_panel("Team Scores by Season", team_season_ui("team", app_data)),
  nav_panel("Weight Class Brackets", brackets_ui("brackets", app_data)),
  nav_panel("Match Finder", match_browser_ui("matches_browse", app_data)),
  nav_spacer(),
  nav_panel("Scoring Notes", methodology_ui("methodology", app_data))
)

server <- function(input, output, session) {
  individual_season_server("individual", app_data)
  careers_server("careers", app_data)
  team_season_server("team", app_data)
  brackets_server("brackets", app_data)
  match_browser_server("matches_browse", app_data)
  methodology_server("methodology", app_data)
}

shinyApp(ui, server)
