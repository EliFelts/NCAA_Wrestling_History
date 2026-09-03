### Build the derived tables the Shiny app renders, from data/*.rds
###
### Run after data-raw/01_pull_sheets.R (or any time the raw data changes),
### from the project root:
###   Rscript data-raw/02_prep_app_data.R
### Output: data/app_tables.rds -- a named list that app.R loads at startup.

library(tidyverse)
library(hms)
library(forcats)
library(conflicted)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

source("R/fn_careers.R")
source("R/fn_ids.R")
source("R/fn_scoring.R")
source("R/fn_placement.R")
source("R/fn_eras.R")

wrestlers_master <- readRDS("data/wrestlers.rds")
matches_master <- readRDS("data/matches.rds")

if (!file.exists("data/reference_tables.rds")) {
  stop("data/reference_tables.rds not found -- run: Rscript data-raw/01_read_reference_xlsx.R")
}
reference_tables <- readRDS("data/reference_tables.rds")

# Prelim (pigtail) team points -----------------------------------------
#
# wrestlers_master$team_points excludes pigtails (methodology.md). The Team
# Scores, Careers, Brackets and Year Rundown tabs have a "count prelim points"
# toggle, so every reconstructed-points column is carried both ways: the base
# (prelim-free) and a `_wp` variant that adds, per pigtail win, current-value
# bonus points + a 1.0 champ / 0.5 consolation advancement point (the same
# rule the official-score path uses). This roll-up is per wrestler-season;
# it joins onto wrestlers_master to make wrestlers_wp, which feeds the lot.
prelim_points_by_season <- matches_master %>%
  filter(
    as.character(round) %in% c("Prelim", "Consolation Prelim"),
    is.na(result) | result != "Bye",
    !is.na(winner_wrestler_id)
  ) %>%
  group_by(wrestler_id = winner_wrestler_id, year, weight_class) %>%
  summarize(
    prelim_points = sum(coalesce(bonus_points, 0) +
      if_else(round == "Prelim", 1, 0.5)),
    .groups = "drop"
  )

wrestlers_wp <- wrestlers_master %>%
  left_join(prelim_points_by_season,
            by = c("wrestler_id", "year", "weight_class")) %>%
  mutate(
    prelim_points = coalesce(prelim_points, 0),
    team_points_wp = coalesce(team_points, 0) + prelim_points
  )

# Individual season table, display-formatted -------------------------------

ind_years_formatted <- wrestlers_master %>%
  mutate(
    display_name = word(wrestler_id, 1, sep = "_", ),
    Record = str_c(wins, losses, sep = "-"),
    Matches = wins + losses,
    `Bonus Percent` = round(bonus / Matches * 100)
  ) %>%
  mutate(
    Name = display_name, Team = team, Weight = weight_class,
    Seed = seed,
    Year = year,
    Placement = placement, `Team Points` = team_points,
    `Bonus Points` = bonus_points, Record,
    Terminations = terminations, Pins = falls,
    Bonus = bonus,
    `Bonus Percent`, Matches, Wins = wins
  ) %>%
  mutate(
    Placement = as_placement(Placement),
    place_rank = placement_rank(Placement),
    Seed = as.factor(seed),
    Seed = fct_na_value_to_level(Seed, "Unseeded")
  )

# Career tables ---------------------------------------------------------------

careers_summary <- build_careers_summary(wrestlers_wp)
careers_formatted <- format_careers(careers_summary) %>%
  mutate(Era = tournament_era(career_start))

# Team tables --------------------------------------------------------------

team_choices <- ind_years_formatted %>%
  distinct(Team) %>%
  arrange(Team) %>%
  pull(Team)

team_results_annual <- wrestlers_wp %>%
  group_by(team, year) %>%
  summarize(
    score = sum(team_points, na.rm = TRUE),
    score_wp = sum(team_points_wp, na.rm = TRUE),
    qualifiers = n(),
    champs = sum(placement == "First", na.rm = TRUE),
    finalists = sum(placement %in% c("First", "Second"), na.rm = TRUE),
    aa = sum(!is.na(placement)),
    bonus_points = sum(bonus_points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    place = min_rank(desc(score)),
    place_wp = min_rank(desc(score_wp))
  ) %>%
  ungroup() %>%
  mutate(era = tournament_era(year))

# Official team scoring --------------------------------------------------
#
# The Year Rundown tab leads with OFFICIAL team scores, not the era-neutral
# reconstruction in `score`. Two sources, tagged in official_source:
#
#   "recorded" -- hand-compiled NCAA final standings (reference_tables$
#      official_standings, from data-raw/team_scores.xlsx). Covers every
#      tournament 1929-2025 except 1931, 1933, 1943-45. Deductions baked in.
#   "computed" -- the leftover years with no recorded row (1928, the war
#      years): sum the per-match official value on matches_master
#      (winner_team_points_secured), add pigtail contributions (see below),
#      apply documented deductions. Validated against the recorded finals --
#      exact to the penny for 2013-2023, within a half-point elsewhere.

team_deductions <- reference_tables$team_deductions

deduction_by_team <- team_deductions %>%
  group_by(year, team) %>%
  summarize(deduction_pts = sum(deduction_pts), .groups = "drop")

official_recorded <- reference_tables$official_standings %>%
  mutate(official_source = "recorded")

recorded_years <- sort(unique(official_recorded$year))

# Pigtail (preliminary-round) points. winner_team_points_secured is NA on
# Prelim / Consolation Prelim rows, so those wins contribute nothing to the
# sum above -- add them back here:
#   * bonus points for the pigtail win -- always (already in `bonus_points`);
#   * an advancement point -- 1.0 champ side / 0.5 consolation. Recent
#     tournaments award this outright for any pigtail win (spot-checked
#     against official finals: 2024 VMI and LIU each scored exactly 1.0 off a
#     1-2 pigtail run -> advancement, no bonus). An older "only if the
#     wrestler wins his next match" rule existed but the changeover year is
#     unknown, so every pigtail win is treated as a normal bracket win.
#     Advancement values come from the bye sheet (champ 1.0 / cons 0.5,
#     populated 1985+; NA before -> bonus only, a safe default -- no year
#     before 2013 is currently "computed" anyway).
pigtail_adv_value <- reference_tables$scoring_values$bye %>%
  filter(criteria == "won_next") %>%
  mutate(adv_value = as.numeric(point_value)) %>%
  filter(!is.na(adv_value)) %>%
  distinct(year, side, adv_value)

pigtail_points <- matches_master %>%
  filter(
    as.character(round) %in% c("Prelim", "Consolation Prelim"),
    is.na(result) | result != "Bye",
    !is.na(winner_wrestler_id), !is.na(winner_team),
    !year %in% recorded_years
  ) %>%
  mutate(side = if_else(round == "Prelim", "champ", "consolation")) %>%
  left_join(pigtail_adv_value, by = c("year", "side")) %>%
  mutate(pigtail_pts = coalesce(bonus_points, 0) + coalesce(adv_value, 0)) %>%
  group_by(year, team = winner_team) %>%
  summarize(pigtail_pts = sum(pigtail_pts), .groups = "drop")

official_computed <- matches_master %>%
  filter(
    !is.na(winner_team_points_secured), !is.na(winner_team),
    !year %in% recorded_years
  ) %>%
  group_by(year, team = winner_team) %>%
  summarize(match_points = sum(winner_team_points_secured), .groups = "drop") %>%
  full_join(pigtail_points, by = c("year", "team")) %>%
  left_join(deduction_by_team, by = c("year", "team")) %>%
  mutate(
    official_score = coalesce(match_points, 0) + coalesce(pigtail_pts, 0) +
      coalesce(deduction_pts, 0),
    official_source = "computed"
  ) %>%
  group_by(year) %>%
  mutate(official_place = min_rank(desc(official_score))) %>%
  ungroup() %>%
  select(year, team, official_place, official_score, official_source)

team_scores_official <- bind_rows(
  official_recorded,
  official_computed
) %>%
  arrange(year, official_place)

team_results_annual <- team_results_annual %>%
  left_join(
    team_scores_official %>%
      select(year, team, official_place, official_score, official_source),
    by = c("year", "team")
  ) %>%
  left_join(deduction_by_team, by = c("year", "team"))

tournament_awards <- reference_tables$tournament_awards

# Weight-class bracket tables ---------------------------------------------
#
# Unit of analysis is the (year, weight_class) pair -- for "best bracket ever"
# comparisons. Two lenses on field strength: what each wrestler had banked
# COMING IN (prior_*), and how their whole CAREER turned out (career_*). Plus a
# "what actually happened" cluster aggregated from the matches.
#
# The career_* lens is built two ways -- best 4 seasons (default, matching the
# careers tab's treatment of the handful of 5-appearance wrestlers) and all
# seasons -- and the app toggles between them. Everything else is identical, so
# only the career_* columns differ across the two variants.

# The season cap ranks on prelim-free team_points (stable across the toggle);
# both career_points and career_points_wp are summed over the chosen seasons.
career_totals_for <- function(w, best_n) {
  w %>%
    group_by(wrestler_id) %>%
    arrange(desc(team_points), .by_group = TRUE) %>%
    filter(row_number() <= best_n) %>%
    summarize(
      career_points = sum(team_points, na.rm = TRUE),
      career_points_wp = sum(team_points_wp, na.rm = TRUE),
      career_resume = sum(placement_to_points(placement)),
      career_aa = sum(!is.na(placement)),
      career_titles = sum(placement %in% "First"),
      career_finalists = sum(placement %in% c("First", "Second")),
      .groups = "drop"
    )
}

# Per-wrestler "coming in" running totals -- unaffected by the season cap.
bracket_field_base <- wrestlers_wp %>%
  arrange(wrestler_id, year) %>%
  group_by(wrestler_id) %>%
  mutate(
    appearance_no = row_number(),
    prior_points = lag(cumsum(coalesce(team_points, 0)), default = 0),
    prior_points_wp = lag(cumsum(coalesce(team_points_wp, 0)), default = 0),
    prior_resume = lag(cumsum(placement_to_points(placement)), default = 0),
    prior_aa = lag(cumsum(!is.na(placement)), default = 0),
    prior_titles = lag(cumsum(placement %in% "First"), default = 0),
    prior_finalists = lag(cumsum(placement %in% c("First", "Second")), default = 0)
  ) %>%
  ungroup() %>%
  mutate(
    Wrestler = id_name(wrestler_id),
    place_rank = placement_rank(placement),
    placement = as_placement(placement)
  )

match_shape <- matches_master %>%
  group_by(year, weight_class) %>%
  summarize(
    matches_n = n(),
    pins = sum(result == "Fall", na.rm = TRUE),
    bonus_rate = round(mean(bonus_points > 0, na.rm = TRUE), 2),
    ot_matches = sum(ot, na.rm = TRUE),
    avg_margin = round(mean(margin2, na.rm = TRUE), 1),
    .groups = "drop"
  )

build_bracket_field <- function(career_totals) {
  bracket_field_base %>%
    left_join(career_totals, by = "wrestler_id") %>%
    select(
      year, weight_class, wrestler_id, Wrestler, team, seed,
      placement, place_rank, season_points = team_points,
      season_points_wp = team_points_wp, appearance_no,
      prior_points, prior_points_wp, prior_resume, prior_aa, prior_titles,
      prior_finalists,
      career_points, career_points_wp, career_resume, career_aa, career_titles,
      career_finalists
    ) %>%
    add_era_cols(year)
}

build_bracket_summary <- function(bracket_field) {
  bracket_field %>%
    group_by(year, weight_class) %>%
    summarize(
      entrants = n(),
      returning_points = sum(prior_points),
      returning_points_wp = sum(prior_points_wp),
      returning_resume = sum(prior_resume),
      returning_aa = sum(prior_aa > 0),
      returning_aa_finishes = sum(prior_aa),
      returning_champs = sum(prior_titles > 0),
      returning_finalists = sum(prior_finalists > 0),
      career_points = sum(career_points),
      career_points_wp = sum(career_points_wp),
      career_resume = sum(career_resume),
      career_aa_finishes = sum(career_aa),
      career_champs = sum(career_titles > 0),
      career_title_total = sum(career_titles),
      .groups = "drop"
    ) %>%
    mutate(
      returning_points_per_entrant = round(returning_points / entrants, 1),
      returning_points_per_entrant_wp = round(returning_points_wp / entrants, 1)
    ) %>%
    left_join(match_shape, by = c("year", "weight_class")) %>%
    add_era_cols(year) %>%
    arrange(desc(returning_resume))
}

bracket_field_best4 <- build_bracket_field(career_totals_for(wrestlers_wp, 4))
bracket_field_all <- build_bracket_field(career_totals_for(wrestlers_wp, Inf))
bracket_summary_best4 <- build_bracket_summary(bracket_field_best4)
bracket_summary_all <- build_bracket_summary(bracket_field_all)

# Consolation-bracket wrestlebacks ("Ultimate Road Warrior") -------------
#
# One row per wrestler-tournament that won at least one match on the
# consolation side. cons_wins is the headline number; first_loss_round says
# how deep the hole was. The canonical road warrior loses early and rattles
# off a long wrestleback string to place. Byes are not wins. Only meaningful
# from the full-wrestleback era (1996+) -- earlier regimes capped how far a
# loser could climb (fn_eras.R::wrestleback_regime()), so the era column
# rides along.

consolation_rounds <- c(
  "Consolation Prelim", "Cons. Round 1", "Cons. Round 2", "Cons. Round 3",
  "Cons. Round 4", "Cons. Round 5", "Cons. Semi",
  "Cons. 2nd Quarterfinal", "Cons. 2nd Semifinal",
  "Cons. 3rd Quarterfinal", "Cons. 3rd Semifinal",
  "7th Place Match", "5th Place Match", "3rd Place Match"
)

real_matches <- matches_master %>%
  filter(is.na(result) | result != "Bye")

# round is an unordered factor, so reduce on its level index and map back.
round_labels <- levels(matches_master$round)

first_loss <- real_matches %>%
  filter(!is.na(loser_wrestler_id)) %>%
  mutate(round_rank = as.integer(round)) %>%
  group_by(year, weight_class, wrestler_id = loser_wrestler_id) %>%
  summarize(first_loss_rank = min(round_rank), .groups = "drop") %>%
  mutate(first_loss_round = factor(round_labels[first_loss_rank], levels = round_labels))

road_warriors <- real_matches %>%
  filter(!is.na(winner_wrestler_id)) %>%
  mutate(is_cons = as.character(round) %in% consolation_rounds) %>%
  group_by(year, weight_class, wrestler_id = winner_wrestler_id) %>%
  summarize(
    cons_wins = sum(is_cons),
    cons_pins = sum(is_cons & result == "Fall", na.rm = TRUE),
    total_wins = n(),
    .groups = "drop"
  ) %>%
  filter(cons_wins > 0) %>%
  left_join(first_loss, by = c("year", "weight_class", "wrestler_id")) %>%
  left_join(
    wrestlers_master %>%
      distinct(year, weight_class, wrestler_id, .keep_all = TRUE) %>%
      select(year, weight_class, wrestler_id, team, seed, placement),
    by = c("year", "weight_class", "wrestler_id")
  ) %>%
  mutate(
    Wrestler = id_name(wrestler_id),
    placement = as_placement(placement),
    place_rank = placement_rank(placement)
  ) %>%
  add_era_cols(year) %>%
  arrange(desc(cons_wins), first_loss_rank, place_rank)

# Bundle --------------------------------------------------------------------

app_data <- list(
  wrestlers_master = wrestlers_master,
  matches_master = matches_master,
  ind_years_formatted = ind_years_formatted,
  careers_summary = careers_summary,
  careers_formatted = careers_formatted,
  team_choices = team_choices,
  team_results_annual = team_results_annual,
  bracket_field_best4 = bracket_field_best4,
  bracket_field_all = bracket_field_all,
  bracket_summary_best4 = bracket_summary_best4,
  bracket_summary_all = bracket_summary_all,
  road_warriors = road_warriors,
  team_scores_official = team_scores_official,
  team_deductions = team_deductions,
  tournament_awards = tournament_awards
)

saveRDS(app_data, "data/app_tables.rds")
message("Wrote data/app_tables.rds (", length(app_data), " tables)")
