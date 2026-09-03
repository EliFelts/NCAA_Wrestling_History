### Read the committed reference workbooks in data-raw/*.xlsx -> data/reference_tables.rds
###
### These are hand-compiled lookups pulled from NCAA records (official team
### scores, documented team-point deductions, tournament awards) plus the
### year-by-year official scoring key. Unlike data-raw/01_pull_sheets.R these
### files need no auth -- they live in the repo -- so this step is cheap to
### re-run. Output is a named list that data-raw/02_prep_app_data.R consumes.
###
### Run from the project root:
###   Rscript data-raw/01_read_reference_xlsx.R

library(tidyverse)
library(readxl)
library(conflicted)
conflicts_prefer(dplyr::filter)

xlsx <- function(file, sheet) read_excel(file.path("data-raw", file), sheet = sheet)

# Team-name spellings in the reference books that differ from the match data's
# canonical team names. Applied (after str_squish) to every reference table
# that carries a team. Historical also-rans with no modern match-data entry
# (Hobart, Oswego State, Chicago, San Francisco, ...) are left as-is -- their
# standings rows are still valid, they just don't enrich with qualifier counts.
team_aliases <- c(
  "NC State" = "North Carolina State",
  "Chattanooga" = "Tennessee-Chattanooga",
  "CSU Bakersfield" = "Cal State-Bakersfield",
  "SIU Edwardsville" = "SIU-Edwardsville",
  "LIU" = "Long Island",
  "Ohio" = "Ohio University",
  "Cal Poly Pomona" = "Cal Poly-Pomona",
  "North Dakota State University" = "North Dakota State",
  "Northwest Missouri State" = "Northwest Missouri",
  "SUNY Maritime" = "SUNY-Maritime College"
)
canon_team <- function(x) {
  x <- str_squish(x)
  hit <- match(x, names(team_aliases))
  ifelse(is.na(hit), x, unname(team_aliases[hit]))
}

# Official final team standings, hand-compiled from NCAA records (deductions
# already baked in). data-raw/team_scores.xlsx covers 1929-2026 -- every
# tournament that happened bar 1928, 1931, 1933, 1943-45, 2020. 2026 is
# excluded until its match data is loaded. Supersedes the old
# ncaa_team_scores.xlsx (1929-2012 only) and NCAA_db.xlsx Sheet2, both left in
# data-raw/ but no longer read.
official_standings <- xlsx("team_scores.xlsx", "official_scores") %>%
  transmute(
    year = as.integer(year),
    team = canon_team(team),
    official_place = as.integer(place),
    official_score = as.numeric(score)
  ) %>%
  filter(!is.na(official_score)) %>%
  # a handful of 1966-1978 years still list "UCLA" twice (a second Cal State
  # school under the same abbreviation); keep the better finish.
  group_by(year, team) %>%
  slice_min(official_place, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year, official_place)

# Documented team-point deductions (unsportsmanlike conduct, mat-area control,
# missed weight, ...), 2011-2025. `amount` is the negative adjustment.
team_deductions <- xlsx("team_scores.xlsx", "deductions") %>%
  transmute(
    year = as.integer(year),
    team = canon_team(team),
    deduction_pts = as.numeric(amount),
    reason = str_squish(replace_na(description, "(unspecified)"))
  ) %>%
  filter(!is.na(deduction_pts), deduction_pts != 0) %>%
  arrange(year, team)

# Outstanding Wrestler (1932-) and Hodge Trophy (1995-).
tournament_awards <- xlsx("ncaa_wrestling_awards.xlsx", "Sheet1") %>%
  transmute(
    year = as.integer(year),
    award,
    wrestler = first_last,
    team = canon_team(team),
    weight = suppressWarnings(as.integer(weight)),
    record
  ) %>%
  arrange(year, award)

# Year-by-year official scoring key + round metadata. Not used by the app yet
# -- staged here for a future "recompute official scores ourselves" pass.
scoring_values <- set_names(excel_sheets("data-raw/ncaa_scoring_values.xlsx")) %>%
  map(~ xlsx("ncaa_scoring_values.xlsx", .x))
round_info <- set_names(excel_sheets("data-raw/ncaa_round_info.xlsx")) %>%
  map(~ xlsx("ncaa_round_info.xlsx", .x))

reference_tables <- list(
  official_standings = official_standings,
  team_deductions = team_deductions,
  tournament_awards = tournament_awards,
  scoring_values = scoring_values,
  round_info = round_info
)

saveRDS(reference_tables, "data/reference_tables.rds")
message(
  "Wrote data/reference_tables.rds -- ",
  nrow(official_standings), " official rows (",
  min(official_standings$year), "-", max(official_standings$year), "), ",
  nrow(team_deductions), " deductions, ",
  nrow(tournament_awards), " awards"
)
