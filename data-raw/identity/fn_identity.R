### Wrestler-identity helpers: match a new tournament's wrestler-seasons against
### the frozen crosswalk (data-raw/wrestler_identity.csv).
###
### The crosswalk is the system of record for "who is a distinct person".
### `individual_id` is a stable surrogate -- it never changes when a wrestler
### returns. The app's "First Last_startYear-endYear" label is a DERIVED display
### string (min/max year per individual_id), rebuilt each time, not stored here.
###
### classify_year()  -- split new seasons into linked / new / review
### resolve_review() -- fold the hand-edited review file back into crosswalk rows

library(tidyverse)

# --- string bits ----------------------------------------------------------

# "First Last (Team)"  ->  "First Last"   (drops one trailing (...) group)
name_from_display <- function(x) str_squish(str_remove(x, "\\s*\\([^()]*\\)\\s*$"))

# the "1999-2002" out of a wrestler_id (tolerates a "_1"/"_2" suffix)
id_range <- function(x) str_extract(x, "[0-9]{4}-[0-9]{4}")

# canonical match key from separate name parts
name_key <- function(first, last) str_squish(str_c(first, " ", last))

# punctuation/spacing/case-insensitive key -- catches drift like
# "Marc-Anthony" vs "Marc Anthony", "O`Dell" vs "O'Dell", "TJ" vs "T.J."
# without collapsing genuinely different names together.
tight_key <- function(name) str_replace_all(str_to_lower(name), "[^a-z0-9]", "")

# first-initial + last-name. Loose on purpose: used only against RECENTLY active
# prior individuals, to catch nickname swaps ("Joey" <-> "Joseph", "Mikey" <->
# "Mike") and to surface siblings on the circuit ("Beau" vs "Brock Mantanona").
initlast_key <- function(name) {
  first <- str_replace_all(word(name, 1), "[^A-Za-z]", "")
  str_c(str_sub(str_to_lower(first), 1, 1), "|", str_to_lower(word(name, -1)))
}

# standard modern weight ladder -- used to measure a weight move in CLASSES, not
# pounds. Historical / non-standard weights return NA and simply aren't scored
# on weight (the gap and team checks still apply).
weight_ladder <- c(125, 133, 141, 149, 157, 165, 174, 184, 197, 285)
class_gap <- function(w_now, w_then) {
  abs(match(w_now, weight_ladder) - match(w_then, weight_ladder))
}

# --- tunables: when a single-candidate match auto-links vs. goes to review ---
identity_thresholds <- list(
  gap_ok   = 3,   # yrs since last appearance still auto-linked (redshirt + COVID)
  span_max = 7,   # first -> current span that stays plausible (Steveson did 7)
  class_ok = 2    # weight-class moves (up or down) that still read as continuity
)

#' Classify one tournament's wrestler-seasons against the crosswalk.
#'
#' @param new_seasons data frame, one row per wrestler-season in the new year.
#'   Must have: name, team, weight_class. Any other columns (seed, placement,
#'   ncaa_wins, wrestler, ...) ride along into the output for review context.
#' @param crosswalk   the data-raw/wrestler_identity.csv contents.
#' @param year        the new tournament year.
#' @param thresholds  see identity_thresholds.
#' @return new_seasons with one row per (season x candidate): status
#'   ("linked" / "new" / "review"), reason, n_candidates, and the matched
#'   individual_id + that individual's prior-appearance summary.
classify_year <- function(new_seasons, crosswalk, year,
                          thresholds = identity_thresholds) {
  stopifnot(all(c("name", "team", "weight_class") %in% names(new_seasons)))

  prior <- crosswalk %>%
    filter(.data$year < .env$year) %>%
    group_by(individual_id, name) %>%
    arrange(year, .by_group = TRUE) %>%
    summarize(
      cand_first_year  = min(year),
      cand_last_year   = max(year),
      cand_last_team   = dplyr::last(team),
      cand_last_weight = dplyr::last(weight_class),
      cand_teams       = str_c(sort(unique(team)), collapse = " | "),
      cand_weights     = str_c(sort(unique(weight_class)), collapse = " | "),
      cand_seasons     = dplyr::n(),
      .groups = "drop"
    )

  prior_tight <- prior %>%
    mutate(tkey = tight_key(name)) %>%
    group_by(tkey) %>%
    summarize(tight_hits = str_c(sort(unique(name)), collapse = " | "),
              tight_ids  = str_c(sort(unique(individual_id)), collapse = " | "),
              .groups = "drop")

  prior_initlast <- prior %>%
    filter(cand_last_year >= year - 3) %>%
    mutate(ilkey = initlast_key(name)) %>%
    group_by(ilkey) %>%
    summarize(il_hits = str_c(sort(unique(str_c(name, " (last ", cand_last_year, ")"))),
                              collapse = " | "),
              il_ids  = str_c(sort(unique(individual_id)), collapse = " | "),
              .groups = "drop")

  base <- new_seasons %>%
    mutate(.season = row_number(), .year = year)

  scored <- base %>%
    left_join(prior, by = "name", relationship = "many-to-many") %>%
    group_by(.season) %>%
    mutate(n_candidates = sum(!is.na(individual_id))) %>%
    ungroup() %>%
    mutate(
      gap_years     = year - cand_last_year,
      weight_delta  = weight_class - cand_last_weight,
      classes_moved = class_gap(weight_class, cand_last_weight),
      team_changed  = !is.na(cand_last_team) & team != cand_last_team,
      span_to_now   = year - cand_first_year,
      auto_link = n_candidates == 1 &
        !is.na(individual_id) &
        gap_years <= thresholds$gap_ok &
        span_to_now <= thresholds$span_max &
        coalesce(classes_moved, 0L) <= thresholds$class_ok
    )

  # nets for a name with NO exact prior match:
  #   tight_conflict - punctuation/case variant of a prior name ("O`Dell")
  #   il_conflict    - same last name + first initial as a prior who was active
  #                    in the last 3 yrs (nickname swap, or a sibling)
  scored <- scored %>%
    mutate(tkey = tight_key(name), ilkey = initlast_key(name)) %>%
    left_join(prior_tight, by = "tkey") %>%
    left_join(prior_initlast, by = "ilkey") %>%
    mutate(
      tight_conflict = n_candidates == 0 & !is.na(tight_hits) & tight_hits != name,
      il_conflict    = n_candidates == 0 & !tight_conflict & !is.na(il_hits),
      status = case_when(
        n_candidates == 0 & (tight_conflict | il_conflict) ~ "review",
        n_candidates == 0                                  ~ "new",
        n_candidates == 1 & auto_link                      ~ "linked",
        TRUE                                               ~ "review"
      ),
      reason = case_when(
        status == "new"          ~ "no prior individual with this name",
        tight_conflict           ~ str_c("punctuation/spacing variant of: ", tight_hits),
        il_conflict              ~ str_c("same last name + initial as recent: ", il_hits, "  (nickname or relative?)"),
        n_candidates >= 2        ~ "name shared by multiple prior individuals",
        gap_years > thresholds$gap_ok ~ str_c(gap_years, "-yr gap since last appearance"),
        span_to_now > thresholds$span_max ~ str_c("span to ", year, " is ", span_to_now, " yrs (past eligibility ceiling)"),
        classes_moved > thresholds$class_ok ~ str_c("moved ", classes_moved, " weight classes (", cand_last_weight, " -> ", weight_class, ")",
                                                    if_else(team_changed, " and changed team", "")),
        status == "linked"       ~ "ok",
        TRUE                     ~ "review (no single rule; check candidate)"
      )
    ) %>%
    select(-tkey, -ilkey)

  # collapse candidate-less rows back to one row; keep every candidate for the
  # multi-candidate review rows
  linked_new <- scored %>% filter(n_candidates <= 1) %>% distinct(.season, .keep_all = TRUE)
  multi      <- scored %>% filter(n_candidates >= 2)
  bind_rows(linked_new, multi) %>% arrange(.season, cand_last_year)
}

#' Turn a hand-resolved review file back into crosswalk rows to append.
#'
#' One decision per wrestler-season (name + team + weight_class + year). A
#' multi-candidate season has one row per candidate -- put the decision on the
#' right row and leave the siblings blank. Each season must resolve to exactly
#' one of:
#'   decision_individual_id  -- link this season to an existing person, or
#'   decision_is_new = TRUE  -- mint a new individual_id for it.
#' New ids continue above max(crosswalk$individual_id). Returns rows shaped like
#' the crosswalk, ready to bind on (see build_wrestlers, the next step).
resolve_review <- function(review, crosswalk) {
  review <- review %>%
    mutate(
      .is_new = tolower(trimws(as.character(decision_is_new))) %in%
        c("true", "t", "yes", "1"),
      .decided = !is.na(decision_individual_id) | .is_new
    )

  decisions <- review %>%
    group_by(name, team, weight_class, year) %>%
    summarize(
      n_decided   = sum(.decided),
      link_id     = { v <- unique(decision_individual_id[.decided & !.is_new]); if (length(v)) v else NA_integer_ },
      n_link_ids  = n_distinct(decision_individual_id[.decided & !.is_new]),
      want_new    = any(.is_new[.decided]),
      seed = first(seed), placement = first(placement), wrestler = first(wrestler),
      .groups = "drop"
    )

  bad <- decisions %>%
    filter(n_decided == 0 | (n_link_ids > 1) | (want_new & n_link_ids >= 1))
  if (nrow(bad)) {
    stop(nrow(bad), " wrestler-season(s) not cleanly resolved ",
         "(need exactly one decision -- an id OR decision_is_new, not both/none):\n",
         paste0("  ", bad$name, " (", bad$team, ", ", bad$weight_class, ")",
                collapse = "\n"))
  }

  next_id <- max(crosswalk$individual_id, na.rm = TRUE)
  decisions %>%
    mutate(
      minted_id = if_else(want_new, next_id + cumsum(want_new), NA_real_),
      individual_id = as.integer(coalesce(as.double(link_id), minted_id)),
      source_wrestler_id = NA_character_,
      origin = str_c("review-", year)
    ) %>%
    select(individual_id, name, team, weight_class, year, seed, placement,
           wrestler, source_wrestler_id, origin)
}
