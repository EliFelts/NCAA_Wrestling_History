### Seed the wrestler-identity crosswalk from the frozen (<= 2025) data.
###
### data/wrestlers.rds is trusted through 2025: its wrestler_id
### ("First Last_startYear-endYear", with a "_1"/"_2" suffix on the rare
### same-name same-range pair) already encodes every manual same-name split made
### during the original build. This script freezes that work into a STABLE
### integer `individual_id` -- one that does not move when a wrestler comes back
### for another year -- and writes one row per wrestler-season to
### data-raw/wrestler_identity.csv, the reference the yearly classifier matches
### each new tournament against.
###
### Run ONCE. The normal yearly flow grows the CSV through
### data-raw/identity/classify_year.R + the hand-resolved review file; it does
### not re-run this (re-seeding would renumber every individual_id). Pass
### FORCE_RESEED=1 in the environment to rebuild from scratch anyway.
###
###   Rscript data-raw/identity/seed_crosswalk.R

library(tidyverse)

out_path <- "data-raw/wrestler_identity.csv"
if (file.exists(out_path) && !nzchar(Sys.getenv("FORCE_RESEED"))) {
  stop(out_path, " already exists. Set FORCE_RESEED=1 to rebuild it from scratch.")
}

source("data-raw/identity/fn_identity.R")   # name_from_display(), id_range()

wrestlers <- readRDS("data/wrestlers.rds")

# One stable id per distinct source wrestler_id (suffix and all). Sorted so the
# numbering is reproducible.
id_lookup <- wrestlers %>%
  distinct(wrestler_id) %>%
  arrange(wrestler_id) %>%
  mutate(individual_id = row_number())

crosswalk <- wrestlers %>%
  left_join(id_lookup, by = "wrestler_id") %>%
  transmute(
    individual_id,
    name = coalesce(na_if(str_squish(first_last), ""), name_from_display(wrestler)),
    team,
    weight_class,
    year,
    seed,
    placement,
    wrestler,
    source_wrestler_id = wrestler_id,
    origin = "seed<=2025"
  ) %>%
  arrange(individual_id, year, weight_class)

# --- manual corrections -----------------------------------------------------
# data/wrestlers.rds merged a handful of same-name people that later turned out
# to be distinct (or vice-versa). Each row of id_corrections.csv overrides
# individual_id for the matching (name, team, year) season(s): use a fresh id
# above the seed's max to split one person out, or an existing id to merge.
corr_path <- "data-raw/identity/id_corrections.csv"
if (file.exists(corr_path)) {
  corr <- read_csv(corr_path, show_col_types = FALSE)
  if (nrow(corr)) {
    seed_max <- max(id_lookup$individual_id)
    key <- function(df, n, t, y) str_c(df[[n]], "\r", df[[t]], "\r", df[[y]])
    hit <- match(
      key(crosswalk, "name", "team", "year"),
      key(corr, "match_name", "match_team", "match_year")
    )
    n_applied <- sum(!is.na(hit))
    if (n_applied == 0) warning("id_corrections.csv: no season rows matched")
    crosswalk <- crosswalk %>%
      mutate(
        .new_id = corr$set_individual_id[hit],
        origin  = if_else(!is.na(.new_id), "correction", origin),
        individual_id = coalesce(.new_id, individual_id)
      ) %>%
      select(-.new_id)
    message("Applied ", n_applied, " id correction row(s) from ", corr_path, ".")
  }
}

crosswalk <- crosswalk %>% arrange(individual_id, year, weight_class)

write_csv(crosswalk, out_path)
message("Wrote ", out_path, " -- ", nrow(crosswalk), " wrestler-seasons, ",
        n_distinct(crosswalk$individual_id), " individuals, through ",
        max(crosswalk$year), ".")

# --- diagnostics: individuals worth an eyeball before trusting the seed -------
# (these are pre-existing quirks in wrestlers.rds, surfaced not fixed)

spans <- crosswalk %>%
  group_by(individual_id, name) %>%
  summarize(first_year = min(year), last_year = max(year),
            seasons = n(), teams = n_distinct(team),
            .groups = "drop") %>%
  mutate(span = last_year - first_year + 1)

wide_span <- spans %>% filter(span >= 7) %>% arrange(desc(span))
if (nrow(wide_span)) {
  message("\n", nrow(wide_span),
          " individual_id(s) span >= 7 calendar years (possible unsplit homonym):")
  print(as.data.frame(wide_span))
}

bad_range <- wrestlers %>%
  mutate(rng = id_range(wrestler_id),
         s = as.integer(str_sub(rng, 1, 4)),
         e = as.integer(str_sub(rng, 6, 9))) %>%
  filter(is.na(rng) | e < s | year < s | year > e) %>%
  select(wrestler, year, wrestler_id)
if (nrow(bad_range)) {
  message("\n", nrow(bad_range),
          " row(s) whose season year sits outside the wrestler_id range:")
  print(as.data.frame(bad_range))
}
