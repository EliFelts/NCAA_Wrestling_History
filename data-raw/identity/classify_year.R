### Classify a new tournament's wrestlers against the identity crosswalk.
###
### Reads the 2026 wrestler-seasons straight off data-raw/tournament_append.R
### (wrestler_scores.join), matches them to data-raw/wrestler_identity.csv, and
### splits them three ways:
###
###   linked  - one prior individual, plausibly continuous  -> auto
###   new     - no prior individual with this name           -> auto
###   review  - everything else (name collisions, big gaps, transfers with a
###             weight jump, spelling variants)              -> human call
###
### Outputs:
###   data-raw/<year>/identity_auto.csv    linked + new, for a sanity scan
###   data-raw/<year>/identity_review.csv  fill decision_individual_id or
###                                        decision_is_new, then feed to
###                                        resolve_review()
###
###   Rscript data-raw/identity/classify_year.R

library(tidyverse)
source("data-raw/identity/fn_identity.R")

YEAR <- 2026

crosswalk <- read_csv("data-raw/wrestler_identity.csv", show_col_types = FALSE)

# --- new tournament's wrestler-seasons ------------------------------------
source("data-raw/tournament_append.R")   # -> wrestler_scores.join, seeds, ...

new_seasons <- wrestler_scores.join %>%
  transmute(
    name = name_key(first_name, last_name),
    team,
    weight_class,
    seed,
    placement,
    ncaa_wins   = wins_ncaa,
    ncaa_losses = losses_ncaa,
    wrestler
  )

res <- classify_year(new_seasons, crosswalk, year = YEAR)

status_by_season <- res %>% distinct(.season, name, status)
cat("\n", YEAR, " wrestler-seasons: ", nrow(new_seasons), "\n", sep = "")
print(count(status_by_season, status))

# --- auto (linked + new) -------------------------------------------------
auto <- res %>%
  filter(status %in% c("linked", "new")) %>%
  transmute(
    year = YEAR, status,
    # only the moves worth eyeballing; a bare 1-yr redshirt gap or a one-class
    # bump is normal and left unflagged
    flag = case_when(
      status != "linked"                                    ~ "",
      team_changed & coalesce(classes_moved, 0L) >= 1       ~ str_c("transfer + ", classes_moved, "-class move"),
      team_changed                                          ~ "transfer",
      coalesce(classes_moved, 0L) >= 2                      ~ str_c(classes_moved, "-class move"),
      gap_years >= 2                                        ~ str_c(gap_years, "-yr gap"),
      TRUE                                                  ~ ""
    ),
    wrestler, name, team, weight_class, seed, placement, ncaa_wins, ncaa_losses,
    individual_id,
    cand_last_year, cand_last_team, cand_last_weight, cand_teams
  ) %>%
  arrange(status, desc(flag != ""), name)

write_csv(auto, file.path("data-raw", YEAR, "identity_auto.csv"))

# --- review ------------------------------------------------------------
review <- res %>%
  filter(status == "review") %>%
  transmute(
    year = YEAR, wrestler, name, team, weight_class, seed,
    placement, ncaa_wins, ncaa_losses,
    reason, n_candidates,
    # candidate id(s) to consider -- from whichever net fired: exact-name match,
    # the nickname/relative net, or the punctuation net
    candidate_ids = case_when(
      n_candidates >= 1  ~ as.character(individual_id),
      !is.na(il_ids)     ~ il_ids,
      !is.na(tight_ids)  ~ tight_ids,
      TRUE               ~ NA_character_
    ),
    candidate_names = case_when(
      n_candidates >= 1  ~ name,
      !is.na(il_hits)    ~ il_hits,
      !is.na(tight_hits) ~ tight_hits,
      TRUE               ~ NA_character_
    ),
    cand_first_year, cand_last_year, cand_last_team, cand_last_weight,
    cand_teams, cand_weights, cand_seasons,
    gap_years, weight_delta, classes_moved, team_changed, span_to_now,
    decision_individual_id = NA_integer_,   # <- existing id to link to, OR
    decision_is_new        = NA,            # <- TRUE for a brand-new person
    notes                  = NA_character_
  ) %>%
  arrange(name, cand_last_year)

review_path <- file.path("data-raw", YEAR, "identity_review.csv")
write_csv(review, review_path)

cat("\nreview rows: ", nrow(review), " (",
    n_distinct(review$name), " wrestlers)\n", sep = "")
if (nrow(review)) {
  print(review %>%
          distinct(name, team, weight_class, reason, n_candidates) %>%
          as.data.frame())
}
cat("\nwrote:\n  ", file.path("data-raw", YEAR, "identity_auto.csv"),
    "\n  ", review_path, "\n", sep = "")
