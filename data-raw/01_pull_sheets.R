### Pull raw tournament data from the Google Sheets and write it to data/*.rds
###
### Run this manually whenever the source sheets change (e.g. after appending a
### new tournament year). Requires googlesheets4 auth. Nothing downstream reads
### the sheets directly -- the app and the prep scripts read data/*.rds only.

library(tidyverse)
library(googlesheets4)
library(hms)
library(conflicted)

conflicts_prefer(
  dplyr::filter,
  dplyr::lag
)

# Seeds -----------------------------------------------------------------------
# Corrected because of a team mismatch in the original join, and a stray
# duplicate row for Lee Roy Smith.

seeds_correct <- read_sheet("https://docs.google.com/spreadsheets/d/1Ot6SxKRJS4OHhIvXXp0pXFKAxs7p5_QDRXfIlLJfwh4/edit?gid=1291266525#gid=1291266525") |>
  filter(!(wrestler == "Lee Roy Smith (Oklahoma State)" & is.na(seed)))

saveRDS(seeds_correct, "data/seeds.rds")

# Matches -------------------------------------------------------------------
# One row per bout across the whole history of the tournament. The round factor
# ordering and the "firstlast" name columns are baked in here so downstream
# code can treat data/matches.rds as ready to use.

matches_master <- read_sheet("https://docs.google.com/spreadsheets/d/1yDlDRlShRcc_aWd_SmDuJ-5naNwhjIQHPVN4UVcpM24/edit?gid=1555277773#gid=1555277773") %>%
  mutate(
    winner_firstlast = str_remove(winner, " \\(.*\\)$"),
    loser_firstlast = str_remove(loser, " \\(.*\\)$"),
    round = factor(round, levels = c(
      "Prelim", "Champ. Round 1", "Champ. Round 2",
      "Consolation Prelim",
      "Cons. Round 1", "Quarterfinal", "Cons. Round 2", "Cons. Round 3",
      "Semifinal", "Cons. Round 4", "Cons. Round 5",
      "Cons. Semi", "7th Place Match",
      "5th Place Match", "3rd Place Match", "1st Place Match",
      "Cons. 2nd Quarterfinal", "Cons. 2nd Semifinal",
      "2nd Place Match", "Cons. 3rd Quarterfinal",
      "Cons. 3rd Semifinal", "Round 1", "Round 2",
      "Round 3", "Round 4", "Round 5", "Round 6",
      "Round 7"
    ))
  )

# Data fixes applied after the read. TODO: correct these in the source sheet,
# then drop the fix.
matches_master <- matches_master %>%
  mutate(
    .fix_rothka = year == 1988 & weight_class == 134 &
      winner_firstlast == "Dan Willaman" & loser_firstlast == "Tim Rothka",
    # recorded 22-60, should be 22-6
    loser_match_points = if_else(.fix_rothka, 6, loser_match_points),
    total_match_points = if_else(
      .fix_rothka, winner_match_points + loser_match_points, total_match_points
    ),
    margin = if_else(
      .fix_rothka, winner_match_points - loser_match_points, margin
    )
  ) %>%
  select(-.fix_rothka)

# 2024 165 lbs pigtails: the champ prelim (Thomsen bt Logan, #33 vs #32) and
# the consolation prelim (Hamilton bt Logan) were parsed with their round
# labels swapped -- which fed Thomsen a consolation advancement point and
# Hamilton a championship one, leaving Northern Iowa 0.5 short and Virginia
# 0.5 over vs the official finals.
matches_master <- matches_master %>%
  mutate(
    .round_chr = as.character(round),
    .round_chr = case_when(
      year == 2024 & weight_class == 165 &
        winner_firstlast == "Jack Thomsen" &
        loser_firstlast == "Jake Logan" ~ "Prelim",
      year == 2024 & weight_class == 165 &
        winner_firstlast == "Nick Hamilton" &
        loser_firstlast == "Jake Logan" ~ "Consolation Prelim",
      TRUE ~ .round_chr
    ),
    round = factor(.round_chr, levels = levels(round))
  ) %>%
  select(-.round_chr)

saveRDS(matches_master, "data/matches.rds")

# Wrestlers ---------------------------------------------------------------
# One row per wrestler per tournament appearance. wrestler_id
# ("First Last_startYear-endYear") is the unique-individual key -- it is built
# upstream in the sheet, NOT here. See data-raw/02_build_wrestlers.R for that
# logic once it is traced back and version controlled.

wrestlers_master <- read_sheet("https://docs.google.com/spreadsheets/d/11TR6yUScjdF4OJYoqiVV4PqruJg2-KXmCCBljk9ABSw/edit?gid=325863048#gid=325863048") %>%
  select(-c(seed)) %>%
  left_join(seeds_correct, by = c(
    "wrestler", "weight_class",
    "year", "team"
  ))

saveRDS(wrestlers_master, "data/wrestlers.rds")
