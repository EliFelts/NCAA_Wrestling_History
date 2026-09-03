### Tab: Year Rundown
###
### Unit of analysis is a single tournament (one year). One year at a time:
### the team leaderboard (OFFICIAL scoring, with the era-neutral
### reconstruction alongside), the ten individual champions, the tournament
### awards (Outstanding Wrestler / Hodge), and two "wildest week" boards --
###
###   Gorriaran            -- most falls in the least cumulative pin time
###   Ultimate Road Warrior -- most wins on the consolation side of the
###                        bracket (lost early, wrestled all the way back)
###
### Official team scores come from data$team_scores_official (recorded NCAA
### finals 1929-2012; computed from match results elsewhere -- flagged).
### Everything else reads existing prepared tables live. Selecting a wrestler
### in champions / Gorriaran / road-warrior drills to their matches.

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(dplyr)

year_rundown_ui <- function(id, data) {
  ns <- NS(id)
  yrs <- sort(unique(data$team_scores_official$year))

  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      helpText(
        "One tournament at a time. The leaderboard leads with official NCAA ",
        "team scores; ", tags$b("Recon. Pts"), " is the era-neutral ",
        "reconstruction used elsewhere in the app."
      ),
      sliderInput(
        ns("year"), "Tournament year",
        min = min(yrs), max = max(yrs),
        value = max(yrs), step = 1, sep = "",
        animate = animationOptions(interval = 1200)
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      value_box("Team champion", textOutput(ns("vb_champ"))),
      value_box("Runner-up", textOutput(ns("vb_runnerup"))),
      value_box("Individual champions", textOutput(ns("vb_ind_champs"))),
      value_box("Falls in the tournament", textOutput(ns("vb_falls")))
    ),
    layout_columns(
      col_widths = c(7, 5, 12, 5, 7, 12),
      card(
        card_header("Team Leaderboard", info_icon(scoring_caveat)),
        textOutput(ns("official_note")),
        DTOutput(ns("teams_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Individual Champions"),
        DTOutput(ns("champs_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Tournament Awards"),
        DTOutput(ns("awards_table")),
        full_screen = TRUE
      ),
      card(
        card_header(
          "Gorriaran Award",
          info_icon(
            tags$p(
              "Goes to the wrestler with the ", tags$b("most falls"), " in the ",
              tags$b("least cumulative time"), " on the mat. Sorted that way; ",
              "pin time is the running total across the wrestler's falls that ",
              "tournament."
            ),
            title = "Gorriaran Award"
          )
        ),
        DTOutput(ns("gorriaran_table")),
        full_screen = TRUE
      ),
      card(
        card_header(
          "Ultimate Road Warrior",
          info_icon(
            tags$p(
              "Most wins on the ", tags$b("consolation side"), " of the bracket ",
              "-- the wrestler who lost early and clawed the furthest back. ",
              tags$b("First Loss"), " is the round the fall to the back side ",
              "happened; earlier is a deeper hole."
            ),
            tags$p(
              "Pre-1996 brackets capped how far a loser could climb, so long ",
              "wrestleback strings are almost entirely a modern-era feat."
            ),
            title = "Ultimate Road Warrior"
          )
        ),
        DTOutput(ns("road_warrior_table")),
        full_screen = TRUE
      ),
      card(
        card_header("Matches for Selected Wrestler"),
        match_table_ui(ns("matches")),
        full_screen = TRUE
      )
    )
  )
}

year_rundown_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # ---- source frames scoped to the chosen year ----------------------

    leaderboard <- reactive({
      req(input$year)
      counts <- data$team_results_annual %>%
        filter(year == input$year) %>%
        select(team, recon_pts = score, qualifiers, champs, finalists, aa)

      data$team_scores_official %>%
        filter(year == input$year) %>%
        left_join(counts, by = "team") %>%
        left_join(
          data$team_deductions %>%
            filter(year == input$year) %>%
            group_by(team) %>%
            summarize(deduction_pts = sum(deduction_pts), .groups = "drop"),
          by = "team"
        ) %>%
        arrange(official_place, desc(official_score))
    })

    champs_year <- reactive({
      req(input$year)
      data$ind_years_formatted %>%
        filter(Year == input$year, place_rank == 1) %>%
        arrange(Weight)
    })

    awards_year <- reactive({
      req(input$year)
      data$tournament_awards %>%
        filter(year == input$year) %>%
        arrange(award)
    })

    gorriaran_year <- reactive({
      req(input$year)
      data$wrestlers_master %>%
        filter(year == input$year, falls > 0, !is.na(falls)) %>%
        mutate(
          pin_secs = suppressWarnings(as.numeric(hms::as_hms(falls_time))),
          pin_secs = ifelse(is.na(pin_secs), Inf, pin_secs)
        ) %>%
        arrange(desc(falls), pin_secs)
    })

    road_warrior_year <- reactive({
      req(input$year)
      data$road_warriors %>%
        filter(year == input$year) %>%
        arrange(desc(cons_wins), first_loss_rank, place_rank)
    })

    # ---- value boxes -------------------------------------------------

    teams_at_rank <- function(lb, n) {
      places <- sort(unique(lb$official_place))
      if (length(places) < n) return("--")
      paste(lb$team[lb$official_place == places[n]], collapse = " / ")
    }

    output$vb_champ <- renderText(teams_at_rank(leaderboard(), 1))
    output$vb_runnerup <- renderText(teams_at_rank(leaderboard(), 2))
    output$vb_ind_champs <- renderText(as.character(nrow(champs_year())))
    output$vb_falls <- renderText({
      n <- sum(
        data$matches_master$year == input$year &
          data$matches_master$result == "Fall",
        na.rm = TRUE
      )
      format(n, big.mark = ",")
    })

    # ---- team leaderboard -----------------------------------------

    output$official_note <- renderText({
      src <- leaderboard()$official_source[1]
      n_ded <- sum(!is.na(leaderboard()$deduction_pts))
      ded_txt <- if (n_ded > 0) {
        sprintf(" %d team-point deduction%s that year (Ded. column).",
                n_ded, if (n_ded == 1) "" else "s")
      } else ""

      base <- if (is.na(src)) {
        "No official team scores recorded for this tournament."
      } else if (src == "recorded") {
        "Official column: recorded NCAA final standings."
      } else {
        paste0(
          "Official column: COMPUTED from match results minus documented ",
          "deductions -- a few points off true finals (more so 2009-2012)."
        )
      }
      paste0(base, ded_txt)
    })

    output$teams_table <- renderDT({
      dat <- leaderboard() %>%
        transmute(
          Place = official_place, Team = team,
          `Off. Score` = official_score,
          `Recon. Pts` = recon_pts,
          # blank the delta for the early years that have no reconstruction
          `Δ` = ifelse(
            is.na(recon_pts) | recon_pts == 0, NA_real_,
            round(recon_pts - official_score, 1)
          ),
          Ded. = deduction_pts,
          Qual = qualifiers, Champs = champs, Fin = finalists, AA = aa
        )
      dt(
        dat,
        selection = "none",
        rownames = FALSE,
        options = list(pageLength = 25, order = list(list(0, "asc")))
      )
    })

    # ---- individual champions + awards ---------------------------

    output$champs_table <- renderDT({
      dat <- champs_year() %>%
        transmute(Weight, Champion = Name, Team, Seed)
      dt(
        dat,
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 15, order = list(list(0, "asc")))
      )
    })

    output$awards_table <- renderDT({
      dat <- awards_year() %>%
        transmute(Award = award, Wrestler = wrestler, Team = team,
                  Weight = weight, Record = record)
      dt(dat, selection = "single", rownames = FALSE, options = list(dom = "t"))
    })

    # ---- Gorriaran ---------------------------------------------

    output$gorriaran_table <- renderDT({
      dat <- gorriaran_year() %>%
        transmute(
          Wrestler = first_last, Team = team, Weight = weight_class,
          Pins = falls, `Pin Time` = falls_time, pin_secs
        )
      # Pin Time is an HH:MM:SS string; sort it (col 4) off hidden pin_secs
      # (col 5) so it orders numerically.
      dt(
        dat,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 15,
          order = list(list(3, "desc"), list(5, "asc")),
          columnDefs = list(
            list(visible = FALSE, targets = 5),
            list(orderData = 5, targets = 4)
          )
        )
      )
    })

    # ---- Ultimate Road Warrior -------------------------------

    output$road_warrior_table <- renderDT({
      dat <- road_warrior_year() %>%
        transmute(
          Wrestler, Team = team, Weight = weight_class, Seed = seed,
          `Cons. Wins` = cons_wins, `Back-side Pins` = cons_pins,
          `First Loss` = as.character(first_loss_round),
          first_loss_rank,
          Placement = as.character(placement), place_rank
        )
      # First Loss (col 6) and Placement (col 8) are labels; sort them off the
      # hidden rank columns (7 and 9).
      dt(
        dat,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 15,
          order = list(list(4, "desc"), list(7, "asc")),
          columnDefs = list(
            list(visible = FALSE, targets = c(7, 9)),
            list(orderData = 7, targets = 6),
            list(orderData = 9, targets = 8)
          )
        )
      )
    })

    # ---- drill-down: last wrestler picked in any of the three ----

    picked <- reactiveVal(NULL)

    observeEvent(input$champs_table_rows_selected, {
      row <- champs_year()[input$champs_table_rows_selected, ]
      picked(list(wid = row$wrestler_id, year = row$Year, weight = row$Weight))
    })
    observeEvent(input$gorriaran_table_rows_selected, {
      row <- gorriaran_year()[input$gorriaran_table_rows_selected, ]
      picked(list(wid = row$wrestler_id, year = row$year, weight = row$weight_class))
    })
    observeEvent(input$road_warrior_table_rows_selected, {
      row <- road_warrior_year()[input$road_warrior_table_rows_selected, ]
      picked(list(wid = row$wrestler_id, year = row$year, weight = row$weight_class))
    })
    observeEvent(input$year, picked(NULL))

    matches_reactive <- reactive({
      p <- picked()
      req(p, p$wid)
      data$matches_master %>%
        filter(
          year == p$year,
          weight_class == p$weight,
          winner_wrestler_id == p$wid | loser_wrestler_id == p$wid
        ) %>%
        arrange(round)
    })

    match_table_server("matches", matches_reactive, show_year = FALSE)

    reactive(picked())
  })
}
