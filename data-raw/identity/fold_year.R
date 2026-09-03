### Fold one tournament year's rows into data/{matches,wrestlers,seeds}.rds.
###
### Run AFTER append_year.R (the crosswalk must already carry <YEAR>). This adds
### the raw rows; then re-run build_wrestlers.R to key them (individual_id +
### derived wrestler_id) and re-stamp matches.
###
### Idempotent: refuses if <YEAR> is already present.
###
###   Rscript data-raw/identity/fold_year.R
###   Rscript data-raw/identity/build_wrestlers.R

library(tidyverse)
source("R/fn_teams.R")

YEAR <- 2026
PIG  <- c("Prelim", "Consolation Prelim")

# match.bind (640 scored bouts) + wrestler_scores.join (330 seeded wrestlers).
# NB: tournament_append.R defines its own `seeds`, `participants`, etc. -- source
# it BEFORE reading the .rds so those names don't clobber ours.
sink(tempfile()); source("data-raw/tournament_append.R"); sink()

matches_rds   <- readRDS("data/matches.rds")
wrestlers_rds <- readRDS("data/wrestlers.rds")
seeds_rds     <- readRDS("data/seeds.rds")
if (any(matches_rds$year == YEAR) || any(wrestlers_rds$year == YEAR)) {
  stop(YEAR, " is already folded into data/*.rds.")
}

no_paren <- function(x) str_squish(str_remove(x, "\\s*\\([^()]*\\)$"))
in_paren <- function(x) str_squish(str_extract(x, "(?<=\\()[^()]+(?=\\)$)"))

# ------------------------------------------------------------- matches.rds
new_matches <- match.bind %>%
  mutate(rd = as.character(round)) %>%
  transmute(
    bout,
    round = factor(rd, levels = levels(matches_rds$round)),
    weight_class,
    winner_wrestler_id = NA_character_,
    winner_team = canon_team(in_paren(winner)),
    winner,
    loser_wrestler_id = NA_character_,
    loser_team = canon_team(in_paren(loser)),
    loser,
    result,
    winner_match_points = as.numeric(winner_match_points),
    loser_match_points  = as.numeric(loser_match_points),
    termination_time,
    ot,
    year = YEAR,
    advancement_value        = if_else(rd %in% PIG, NA_real_, as.numeric(advancement_value)),
    secured_placement_points = if_else(rd %in% PIG, NA_real_, as.numeric(secured_placement_points)),
    bonus_points = as.numeric(bonus),
    winner_team_points_secured = if_else(rd %in% PIG, NA_real_, as.numeric(winner_team_points_secured)),
    total_match_points = coalesce(winner_match_points, 0) + coalesce(loser_match_points, 0),
    margin  = winner_match_points - loser_match_points,
    margin2 = case_when(
      result %in% c("Fall", "Technical Fall")     ~ 15,
      result %in% c("Decision", "Major Decision") ~ winner_match_points - loser_match_points,
      TRUE                                        ~ NA_real_
    ),
    winner_firstlast = no_paren(winner),
    loser_firstlast  = no_paren(loser)
  )

# identity columns (winner_individual_id, ...) are (re)built by build_wrestlers.R;
# match against the schema minus those so this works before or after a build
core_mcols <- setdiff(names(matches_rds),
                      c("winner_individual_id", "loser_individual_id"))
stopifnot(setequal(names(new_matches), core_mcols), !anyNA(new_matches$round))
new_matches <- new_matches[core_mcols]

# ------------------------------------------------------------ wrestlers.rds
hms_str <- function(sec) sprintf("%02d:%02d:%02d",
                                 sec %/% 3600L, (sec %% 3600L) %/% 60L, sec %% 60L)
mb <- match.bind %>%
  mutate(
    rd  = as.character(round),
    wfl = no_paren(winner), lfl = no_paren(loser),
    tsec = suppressWarnings(
      as.integer(str_extract(termination_time, "^\\d+")) * 60L +
      as.integer(str_extract(termination_time, "\\d+$")))
  )

BONUS   <- c("Fall", "Technical Fall", "Major Decision",
             "Injury Default", "Medical Forfeit", "Disqualification")
TERMIN  <- setdiff(BONUS, "Major Decision")

win_stats <- mb %>%
  group_by(fl = wfl) %>%
  summarize(
    wins           = n(),
    wins_noprelim  = sum(!rd %in% PIG),
    falls          = sum(result == "Fall"),
    falls_noprelim = sum(result == "Fall" & !rd %in% PIG),
    techs          = sum(result == "Technical Fall"),
    terminations   = sum(result %in% TERMIN),
    bonus          = sum(result %in% BONUS),
    falls_sec      = sum(tsec[result == "Fall"], na.rm = TRUE),
    tech_sec       = sum(tsec[result == "Technical Fall"], na.rm = TRUE),
    .groups = "drop"
  )
lose_stats <- mb %>%
  group_by(fl = lfl) %>%
  summarize(losses = n(), losses_noprelim = sum(!rd %in% PIG), .groups = "drop")

new_wrestlers <- wrestler_scores.join %>%
  transmute(
    wrestler, weight_class, first_name, last_name,
    team = canon_team(team),
    year = YEAR,
    first_last = str_squish(str_c(first_name, " ", last_name)),
    wrestler_id = NA_character_,        # build_wrestlers.R fills this
    placement,
    team_points  = as.numeric(team_points),
    bonus_points = as.numeric(bonus_points),
    seed = as.numeric(seed)
  ) %>%
  left_join(win_stats,  by = c("first_last" = "fl")) %>%
  left_join(lose_stats, by = c("first_last" = "fl")) %>%
  mutate(
    across(c(wins, wins_noprelim, falls, falls_noprelim, techs, terminations,
             bonus, losses, losses_noprelim), ~ as.numeric(replace_na(., 0L))),
    falls_time = hms_str(replace_na(falls_sec, 0L)),
    tech_time  = hms_str(replace_na(tech_sec, 0L))
  ) %>%
  select(-falls_sec, -tech_sec)

want <- setdiff(names(wrestlers_rds), "individual_id")   # build_wrestlers.R re-adds it
missing_cols <- setdiff(want, names(new_wrestlers))
if (length(missing_cols)) stop("new_wrestlers missing: ", toString(missing_cols))
new_wrestlers <- new_wrestlers[want]

# ---------------------------------------------------------------- seeds.rds
new_seeds <- wrestler_scores.join %>%
  transmute(wrestler, weight_class, team = canon_team(team),
            year = YEAR, seed = as.numeric(seed))
new_seeds <- new_seeds[names(seeds_rds)]

# -------------------------------------------------------------------- write
# bind_rows fills any identity columns present on the old rows with NA for the
# new ones; build_wrestlers.R (next) rebuilds them across the board
saveRDS(bind_rows(matches_rds, new_matches),   "data/matches.rds")
saveRDS(bind_rows(wrestlers_rds, new_wrestlers), "data/wrestlers.rds")
saveRDS(bind_rows(seeds_rds, new_seeds),       "data/seeds.rds")

message("Folded ", YEAR, ":")
message("  matches.rds   += ", nrow(new_matches),   " -> ", nrow(matches_rds) + nrow(new_matches))
message("  wrestlers.rds += ", nrow(new_wrestlers), " -> ", nrow(wrestlers_rds) + nrow(new_wrestlers))
message("  seeds.rds     += ", nrow(new_seeds),     " -> ", nrow(seeds_rds) + nrow(new_seeds))
message("Now run: Rscript data-raw/identity/build_wrestlers.R")
