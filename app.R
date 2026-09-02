### NCAA Wrestling Tournament History -- Shiny app
###
### Data prep lives elsewhere:
###   data-raw/01_pull_sheets.R     pulls the Google Sheets -> data/*.rds
###   data-raw/02_prep_app_data.R   builds the derived tables -> data/app_tables.rds
### This file is UI + server only. Run data-raw/02_prep_app_data.R if data/ is stale.

library(tidyverse)
library(bslib)
library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(conflicted)
library(bsicons)
library(fontawesome)
library(hms)
library(forcats)

conflicts_prefer(
  DT::renderDT,
  dplyr::filter,
  dplyr::lag,
  plotly::layout
)

# Load the prepared tables into the global environment:
#   wrestlers_master, matches_master, ind_years_formatted, careers_summary,
#   careers_formatted, team_choices, team_results_annual
if (!file.exists("data/app_tables.rds")) {
  stop("data/app_tables.rds not found -- run: Rscript data-raw/02_prep_app_data.R")
}
list2env(readRDS("data/app_tables.rds"), envir = environment())

# build user interface

ui <- page_navbar(
  title = "NCAA Wrestling Tournament Results",
  theme = bs_theme(preset = "cerulean"),
  id = "nav",
  sidebar = sidebar(
    width = 500,
    conditionalPanel(
      "input.nav==`Individual Season Data`",
      accordion(
        accordion_panel(
          "Explore Data",
          sliderInput(
            inputId = "ind_dates",
            label = "Choose a range of years",
            min = min(wrestlers_master$year),
            max = max(wrestlers_master$year),
            value = c(1980, max(wrestlers_master$year)),
            sep = ""
          ),
          pickerInput(
            inputId = "team_filter",
            label = "Filter by Team",
            choices = team_choices,
            selected = team_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          ),
          pickerInput(
            inputId = "placement_filter",
            label = "Filter by Placement",
            choices = levels(ind_years_formatted$Placement),
            selected = levels(ind_years_formatted$Placement),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE
            )
          ),
          pickerInput(
            inputId = "seed_filter",
            label = "Filter by Seed",
            choices = levels(ind_years_formatted$Seed),
            selected = levels(ind_years_formatted$Seed),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE
            )
          )
        )
      )
    ),
    conditionalPanel(
      "input.nav==`Individual Career Data`",
      accordion(
        accordion_panel(
          "Filter Careers",
          sliderInput(
            inputId = "career_dates",
            label = "Choose a range of years",
            min = min(wrestlers_master$year),
            max = max(wrestlers_master$year),
            value = c(1980, max(careers_summary$career_end)),
            sep = ""
          ),
          pickerInput(
            inputId = "career_team_filter",
            label = "Filter by Team",
            choices = team_choices,
            selected = team_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          )
        )
      )
    ),
    conditionalPanel(
      "input.nav==`Team Scores by Season`",
      accordion(
        accordion_panel(
          "Filter Team Races",
          sliderInput(
            inputId = "teamrace_years",
            label = "Choose years",
            min = min(team_results_annual$year),
            max = max(team_results_annual$year),
            value = c(1980, max(team_results_annual$year)),
            sep = ""
          ),
          pickerInput(
            inputId = "teamrace_team_filter",
            label = "Filter by Team",
            choices = team_choices,
            selected = team_choices,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Individual Season Data",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6),
        card(

          card_header("Individual Tournament Summaries"),
          DTOutput("ind_tourneys_table"),
          full_screen = TRUE
        ),
        card(

          card_header("Individual Tournament Matches by Selection"),
          DTOutput("ind_matches_table"),
          full_screen = TRUE
        )
      )
    )
  ),
  nav_panel(
    "Individual Career Data",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6, 6),
        card(

          card_header("Career Summaries"),
          DTOutput("careers_table"),
          full_screen = TRUE
        ),
        card(
          card_header("Individual Seasons for Selected Wrestler"),
          DTOutput("seasons_careerfilter_table"),
          full_screen = TRUE
        ),
        card(
          card_header("Matches for Selected Wrestler"),
          DTOutput("career_matches_table"),
          full_screen = TRUE
        )
      ),
    )
  ),
  nav_panel(
    "Team Scores by Season",
    page_fillable(
      layout_columns(
        col_widths = c(6, 6, 6),
        card(

          card_header("Team Scores by Year"),
          DTOutput("teamscores_table"),
          full_screen = TRUE
        ),
        card(

          card_header("Individual Performances from Selected Team"),
          DTOutput("teamseasons_careerfilter_table"),
          full_screen = TRUE
        ),
        card(

          card_header("Individual Matches from Selected Team"),
          DTOutput("teammatches_filter_table"),
          full_screen = TRUE
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # make the individual tournaments by wrestlers filter reactively

  ind_tourneys_reactive <- reactive({
    req(input$ind_dates)

    ind_tourney_min <- min(input$ind_dates)
    ind_tourney_max <- max(input$ind_dates)

    ind_tourney.dat <- ind_years_formatted %>%
      filter(
        Year >= ind_tourney_min,
        Year <= ind_tourney_max,
        Placement %in% input$placement_filter,
        Seed %in% input$seed_filter
      ) %>%
      filter(Team %in% input$team_filter) %>%
      arrange(desc(`Team Points`))
  })



  # render a data table based on filters

  output$ind_tourneys_table <- renderDT({
    dat <- ind_tourneys_reactive() %>%
      select(Name, Team, Weight, Seed, Year, Placement, `Team Points`, `Bonus Percent`)

    datatable(
      dat,
      selection = "single",
      options = list(pageLength = 25)
    )
  })

  # filter individual matches based on a selected individual
  # from the individual tourneys table, first create
  # a reactive object of the selected row (right now
  # only allowing a single selection)

  tourneys_reactive <- reactive({
    tourney_dat <- ind_tourneys_reactive()
    selected_tourneys <- input$ind_tourneys_table_rows_selected

    dat <- tourney_dat[selected_tourneys, ]
  })

  # now filter the data reactively

  matches_reactive <- reactive({
    req(tourneys_reactive())

    dat <- tourneys_reactive()

    output <- matches_master %>%
      filter(
        winner_wrestler_id %in% dat$wrestler_id | loser_wrestler_id %in% dat$wrestler_id,
        year %in% dat$Year,
        weight_class %in% dat$Weight
      ) %>%
      arrange(round)
  })

  # make the filtered matches for selected individual render
  # to a datatable object

  output$ind_matches_table <- renderDT({
    req(tourneys_reactive())
    req(matches_reactive())

    dat <- matches_reactive() %>%
      mutate(
        Winner = str_c(winner_firstlast, winner_team, sep = " - "),
        Loser = str_c(loser_firstlast, loser_team, sep = " - "),
        Score = str_c(winner_match_points, loser_match_points, sep = "-")
      ) %>%
      select(
        Round = round, Weight = weight_class,
        Winner, Result = result,
        Loser, Score,
        `Termination Time` = termination_time,
        `Team Points Secured` = winner_team_points_secured
      )

    datatable(
      dat
    )
  })


  # make careers by wrestlers filter reactively

  careers_reactive <- reactive({
    req(input$career_dates)

    teams_selected <- input$career_team_filter
    team_pattern <- paste0(
      "(^|,\\s*)(", # start of string or “comma+optional spaces”
      paste0(teams_selected, collapse = "|"), # Iowa|Oklahoma State
      ")(?=,|$)" # followed by comma or end-of-string
    )


    career_min <- min(input$career_dates)
    career_max <- max(input$career_dates)

    career.dat <- careers_formatted %>%
      filter(
        career_start >= career_min,
        career_end <= career_max,
        str_detect(`Team(s)`, regex(team_pattern))
      ) %>%
      arrange(desc(`Team Points`))
  })



  # render a data table based on filters

  output$careers_table <- renderDT({
    dat <- careers_reactive() %>%
      select(-c(
        wrestler_id, career_start, career_end,
        Falls, `Total Falls Time`, `Bonus Wins`
      ))

    datatable(dat,
      filter = "top",
      selection = "single",
      options = list(pageLength = 25)
    )
  })

  # filter individual years based on a selected individual
  # careers from the individual career table, first create
  # a reactive object of the selected row (right now
  # only allowing a single selection)

  seasons_reactive <- reactive({
    career_dat <- careers_reactive()
    selected_careers <- input$careers_table_rows_selected

    dat <- career_dat[selected_careers, ]
  })

  # now filter the season reactively

  seasons_careerfilter_reactive <- reactive({
    req(seasons_reactive())

    dat <- seasons_reactive()

    output <- ind_years_formatted %>%
      filter(wrestler_id %in% dat$wrestler_id)
  })

  # make the filtered seasons for selected individual render
  # to a datatable object

  output$seasons_careerfilter_table <- renderDT({
    # dat <- careers_reactive()

    req(seasons_reactive())
    req(seasons_careerfilter_reactive())

    dat <- seasons_careerfilter_reactive() %>%
      select(Name, Team, Weight, Seed, Year, Placement, `Team Points`, `Bonus Percent`)

    datatable(
      dat,
      selection = "single",
      options = list(pageLength = 25)
    )
  })

  # filter individual matches based on a selected individual
  # from the career tourneys table, first create
  # a reactive object of the selected row (right now
  # only allowing a single selection)

  career_matches_reactive <- reactive({
    req(seasons_reactive())

    dat <- seasons_reactive()

    output <- matches_master %>%
      filter(winner_wrestler_id %in% dat$wrestler_id | loser_wrestler_id %in% dat$wrestler_id) %>%
      arrange(year, round)
  })


  # make the filtered matches for selected careers render
  # to a datatable object

  output$career_matches_table <- renderDT({
    req(seasons_reactive())

    dat <- career_matches_reactive() %>%
      mutate(
        Winner = str_c(winner_firstlast, winner_team, sep = " - "),
        Loser = str_c(loser_firstlast, loser_team, sep = " - "),
        Score = str_c(winner_match_points, loser_match_points, sep = "-")
      ) %>%
      select(
        Year = year, Round = round, Weight = weight_class,
        Winner, Result = result,
        Loser, Score,
        `Termination Time` = termination_time,
        `Team Points Secured` = winner_team_points_secured
      )

    datatable(

      dat,
      options = list(pageLength = 25)
    )
  })

  # make a reactive team score object

  teamscores_reactive <- reactive({
    team_results_annual %>%
      filter(
        year >= min(input$teamrace_years),
        year <= max(input$teamrace_years),
        team %in% input$teamrace_team_filter
      ) %>%
      arrange(-score)
  })

  # make the filtered team score table
  # render as a Data Table

  output$teamscores_table <- renderDT({
    dat <- teamscores_reactive() %>%
      rename(
        Team = team, Year = year, Score = score,
        Qualifiers = qualifiers, Champs = champs,
        Finalists = finalists, AA = aa,
        `Bonus Points` = bonus_points
      )

    datatable(

      dat,
      selection = list(mode = "single"),
      options = list(pageLength = 25)
    )
  })

  # filter individual years based on a selected team
  # careers from the individual career table, first create
  # a reactive object of the selected row (right now
  # only allowing a single selection)

  teamseasons_reactive <- reactive({
    teamscores_dat <- teamscores_reactive()
    selected_seasons <- input$teamscores_table_rows_selected

    dat <- teamscores_dat[selected_seasons, ]
  })

  # now filter the individual seasons reactively

  teamseasons_careerfilter_reactive <- reactive({
    req(teamseasons_reactive())

    dat <- teamseasons_reactive()

    output <- ind_years_formatted %>%
      filter(
        team %in% dat$team,
        year %in% dat$year
      )
  })

  # make the filtered team seasons for selected individual render
  # to a datatable object

  output$teamseasons_careerfilter_table <- renderDT({
    # dat <- careers_reactive()

    req(teamseasons_reactive())
    req(teamseasons_careerfilter_reactive())

    dat <- teamseasons_careerfilter_reactive() %>%
      select(Name, Team, Weight, Seed, Year, Placement, `Team Points`, `Bonus Percent`) %>%
      arrange(Weight)

    datatable(
      dat,
      selection = "single",
      options = list(pageLength = 25)
    )
  })

  # filter matches reactively from the selected team/year combo

  team_seasons_matchfilter_reactive <- reactive({
    req(teamseasons_reactive())

    dat <- teamseasons_reactive()

    output <- matches_master %>%
      filter(
        winner_team %in% dat$team | loser_team %in% dat$team,
        year %in% dat$year
      )
  })

  # render a data table of all the individual matches for the
  # team/year selection

  output$teammatches_filter_table <- renderDT({
    req(teamseasons_reactive())

    dat <- team_seasons_matchfilter_reactive() %>%
      mutate(
        Winner = str_c(winner_firstlast, winner_team, sep = " - "),
        Loser = str_c(loser_firstlast, loser_team, sep = " - "),
        Score = str_c(winner_match_points, loser_match_points, sep = "-")
      ) %>%
      select(
        Year = year, Round = round, Weight = weight_class,
        Winner, Result = result,
        Loser, Score,
        `Termination Time` = termination_time,
        `Team Points Secured` = winner_team_points_secured
      )

    datatable(

      dat,
      options = list(pageLength = 25)
    )
  })
}

shinyApp(ui, server)
