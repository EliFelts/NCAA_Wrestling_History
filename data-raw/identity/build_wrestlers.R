### Rebuild data/wrestlers.rds and re-stamp data/matches.rds from the identity
### crosswalk.
###
### The crosswalk (data-raw/wrestler_identity.csv) is the source of truth for
### WHO IS WHO -- a stable integer `individual_id` per person. This script:
###
###   1. attaches individual_id to every wrestler-season and every match side
###   2. derives wrestler_id = "First Last_startYear-endYear" fresh, from the
###      seasons actually present (with a _1/_2 suffix if two people collide),
###      so it stays a pure display label, not a key
###
### Counting stats (wins, falls, team_points, ...) are carried through untouched
### -- they are trusted. Run this after fold_year.R has added a new tournament's
### rows (they arrive with wrestler_id = NA); it keys them and widens the range
### on every returning wrestler. Safe to re-run any time.
###
###   Rscript data-raw/identity/build_wrestlers.R

library(tidyverse)
source("data-raw/identity/fn_identity.R")   # tight_key()

strip_range <- function(x) str_remove(x, "_[0-9]{4}-[0-9]{4}(_[0-9]+)?$")

crosswalk <- read_csv("data-raw/wrestler_identity.csv", show_col_types = FALSE)
# re-runnable: identity columns from a previous run are rebuilt, not trusted
wrestlers <- readRDS("data/wrestlers.rds") %>% select(-any_of("individual_id"))
matches   <- readRDS("data/matches.rds") %>%
  select(-any_of(c("winner_individual_id", "loser_individual_id")))

# ------------------------------------------------------------------ identity
# canonical display name per individual: the name their pre-existing wrestler_id
# used (all the crosswalk's pre-2026 rows for an id carry the same
# source_wrestler_id); brand-new individuals fall back to their lone season name
canon <- crosswalk %>%
  group_by(individual_id) %>%
  summarize(
    canonical_name = {
      s <- source_wrestler_id[!is.na(source_wrestler_id)]
      if (length(s)) strip_range(s[[1]]) else name[[1]]
    },
    .groups = "drop"
  )

# ------------------------------------------------------------- wrestlers.rds
# attach individual_id, two keys:
#   1. original wrestler_id + year + weight_class  -- unique even for the rare
#      same-name/same-year/same-weight pair; matches untouched <=2025 rows
#   2. name + year + weight_class                  -- for folded-in rows (no
#      wrestler_id yet) and split rows (whose wrestler_id this script rewrote,
#      breaking key 1); ambiguous tuples are dropped, key 1 covers them
xwalk_src <- crosswalk %>%
  filter(!is.na(source_wrestler_id)) %>%
  distinct(source_wrestler_id, year, weight_class, ii1 = individual_id)

ambig <- crosswalk %>% distinct(name, year, weight_class, individual_id) %>%
  count(name, year, weight_class) %>% filter(n > 1) %>% select(-n)
xwalk_season <- crosswalk %>%
  anti_join(ambig, by = c("name", "year", "weight_class")) %>%
  distinct(name, year, weight_class, ii2 = individual_id)

w <- wrestlers %>%
  left_join(xwalk_src,
            by = c("wrestler_id" = "source_wrestler_id", "year", "weight_class")) %>%
  left_join(xwalk_season,
            by = c("first_last" = "name", "year", "weight_class")) %>%
  mutate(individual_id = coalesce(ii1, ii2)) %>%
  select(-ii1, -ii2)

miss <- w %>% filter(is.na(individual_id))
if (nrow(miss)) {
  stop(nrow(miss), " wrestlers.rds row(s) did not match the crosswalk, e.g.:\n",
       paste0("  ", head(miss$wrestler, 8), " ", head(miss$year, 8), collapse = "\n"))
}
stopifnot(nrow(w) == nrow(wrestlers))

# Keep every existing wrestler_id byte-for-byte EXCEPT where identity changed:
#   - a folded-in row (wrestler_id NA)  -> mint / widen the range
#   - an old id now covering two people (split), or one person under two old
#     ids (merge)                       -> fresh string, _1/_2 de-collided
id_map <- w %>% filter(!is.na(wrestler_id)) %>% distinct(individual_id, old_wid = wrestler_id)
split_ids <- id_map %>% group_by(old_wid)       %>% filter(n_distinct(individual_id) > 1) %>% pull(individual_id)
merge_ids <- id_map %>% group_by(individual_id) %>% filter(n_distinct(old_wid) > 1)       %>% pull(individual_id)
new_ids   <- w %>% filter(is.na(wrestler_id)) %>% pull(individual_id)
touched   <- reduce(list(split_ids, merge_ids, new_ids), union)

# collision check is only against ids that stay put
existing_ids <- w %>% filter(!individual_id %in% touched, !is.na(wrestler_id)) %>%
  pull(wrestler_id) %>% unique()
id_range <- if (!length(touched)) {
  tibble(individual_id = integer(), wrestler_id_new = character())
} else {
  w %>%
    filter(individual_id %in% touched) %>%
    group_by(individual_id) %>%
    summarize(start = min(year), end = max(year), .groups = "drop") %>%
    left_join(canon, by = "individual_id") %>%
    mutate(base = str_c(canonical_name, "_", start, "-", end)) %>%
    group_by(base) %>%
    mutate(wrestler_id_new = if (n() == 1 && !base[[1]] %in% existing_ids) base
           else str_c(base, "_", seq_len(n())[order(individual_id)])) %>%
    ungroup() %>%
    select(individual_id, wrestler_id_new)
}

w <- w %>%
  left_join(id_range, by = "individual_id") %>%
  mutate(wrestler_id = coalesce(wrestler_id_new, wrestler_id)) %>%
  select(-wrestler_id_new) %>%
  relocate(individual_id, .before = wrestler_id)

changed <- tibble(old = wrestlers$wrestler_id, new = w$wrestler_id,
                  year = w$year, ii = w$individual_id) %>%
  filter(!is.na(old) & old != new)
message("wrestlers.rds: ", nrow(w), " rows, ", n_distinct(w$individual_id),
        " individuals; wrestler_id changed on ", nrow(changed), " existing row(s) / ",
        n_distinct(changed$ii), " individuals; ", sum(is.na(wrestlers$wrestler_id)),
        " folded row(s) newly keyed.")
if (nrow(changed)) {
  print(changed %>% distinct(old, new) %>%
          mutate(kind = if_else(str_detect(old, "_\\d{4}-\\d{4}$") &
                                str_sub(old, -4) != str_sub(new, -4),
                                "range widened", "other")) %>%
          count(kind))
  print(changed %>% distinct(old, new) %>%
          filter(!(str_detect(old, "-2025$") & str_detect(new, "-2026$"))) %>%
          head(20))
}

saveRDS(w, "data/wrestlers.rds")

# --------------------------------------------------------------- matches.rds
# re-key each side to individual_id / wrestler_id: exact (first_last, team,
# year) first, then a punctuation-insensitive fallback for the ~dozen name
# strings that differ across the two files (J`Den vs J'Den, ...).
exact_key <- w %>% distinct(first_last, team, year, individual_id, wrestler_id)
tight_lu  <- w %>%
  mutate(tk = tight_key(first_last)) %>%
  distinct(tk, team, year, .keep_all = TRUE) %>%   # drop the rare tk collision
  distinct(tk, team, year, ii = individual_id, wid = wrestler_id)

restamp <- function(m, who) {
  fl <- str_c(who, "_firstlast"); tm <- str_c(who, "_team"); tk <- str_c(who, "_tk")
  m %>%
    left_join(exact_key,
              by = setNames(c("first_last", "team", "year"), c(fl, tm, "year"))) %>%
    left_join(tight_lu,
              by = setNames(c("tk", "team", "year"), c(tk, tm, "year"))) %>%
    mutate(
      "{who}_individual_id" := coalesce(individual_id, ii),
      "{who}_wrestler_id"   := coalesce(wrestler_id, wid, .data[[str_c(who, "_wrestler_id")]])
    ) %>%
    select(-individual_id, -wrestler_id, -ii, -wid)
}

m2 <- matches %>%
  mutate(winner_tk = tight_key(winner_firstlast),
         loser_tk  = tight_key(loser_firstlast)) %>%
  restamp("winner") %>% restamp("loser") %>%
  select(-winner_tk, -loser_tk)

real      <- m2 %>% filter(is.na(result) | result != "Bye")
unmatched <- real %>% filter(is.na(winner_individual_id))
message("matches.rds: ", nrow(m2), " rows; winner_individual_id set on ",
        sum(!is.na(m2$winner_individual_id)), ", loser on ",
        sum(!is.na(m2$loser_individual_id)),
        "; ", nrow(unmatched), " non-bye rows still with no winner id:")
if (nrow(unmatched)) {
  print(unmatched %>% count(year, winner_firstlast, winner_team, sort = TRUE),
        n = Inf)
}
stopifnot(nrow(m2) == nrow(matches))

saveRDS(m2, "data/matches.rds")
message("done.")
