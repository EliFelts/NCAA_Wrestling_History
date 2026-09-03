### Append one classified + resolved tournament year to wrestler_identity.csv.
###
### Inputs (all for <YEAR>):
###   data-raw/<YEAR>/identity_auto.csv     linked + new, auto-decided
###   data-raw/<YEAR>/identity_review.csv   hand-resolved (run check_review.R first)
### Effect: grows data-raw/wrestler_identity.csv by one row per wrestler-season.
### Idempotency: refuses if <YEAR> is already in the crosswalk.
###
###   Rscript data-raw/identity/append_year.R

library(tidyverse)
source("data-raw/identity/fn_identity.R")

YEAR    <- 2026
cw_path <- "data-raw/wrestler_identity.csv"

crosswalk <- read_csv(cw_path, show_col_types = FALSE)
if (any(crosswalk$year == YEAR)) {
  stop(YEAR, " is already in ", cw_path, " (", sum(crosswalk$year == YEAR),
       " rows). Nothing to append.")
}
known_ids <- crosswalk$individual_id
seed_max  <- max(known_ids)

auto   <- read_csv(file.path("data-raw", YEAR, "identity_auto.csv"),   show_col_types = FALSE)
review <- read_csv(file.path("data-raw", YEAR, "identity_review.csv"), show_col_types = FALSE)
review$decision_individual_id <- suppressWarnings(as.integer(review$decision_individual_id))

# --- review rows: link or mint (resolve_review mints from seed_max) ---------
resolved <- resolve_review(review, crosswalk)

# --- auto rows: linked keep their id; "new" get ids continuing past the -----
#     ids resolve_review just minted
mint_from <- max(c(known_ids, resolved$individual_id))
auto_new_ids <- auto %>%
  filter(status == "new") %>%
  distinct(name, team, weight_class) %>%
  mutate(minted_id = mint_from + row_number())

auto_rows <- auto %>%
  left_join(auto_new_ids, by = c("name", "team", "weight_class")) %>%
  mutate(individual_id = coalesce(individual_id, minted_id)) %>%
  transmute(individual_id, name, team, weight_class, year = YEAR,
            seed, placement, wrestler)

# --- combine, shape to the crosswalk schema -------------------------------
new_rows <- bind_rows(auto_rows, resolved %>% select(names(auto_rows))) %>%
  mutate(
    source_wrestler_id = NA_character_,
    origin = if_else(individual_id %in% known_ids, "linked-2026", "new-2026")
  )

# --- sanity ------------------------------------------------------------
n_field <- nrow(distinct(new_rows, name, team, weight_class))
stopifnot(
  nrow(new_rows) == n_field,                     # one row per wrestler-season
  !anyNA(new_rows$individual_id),
  all(new_rows$individual_id > 0)
)
dup_id <- new_rows %>%
  filter(origin == "new-2026") %>%
  filter(individual_id %in% known_ids)
if (nrow(dup_id)) stop("new-2026 rows collide with existing ids:\n",
                       paste(dup_id$name, collapse = ", "))

updated <- bind_rows(crosswalk, new_rows) %>%
  arrange(individual_id, year, weight_class)

write_csv(updated, cw_path)

message("Appended ", nrow(new_rows), " ", YEAR, " wrestler-seasons to ", cw_path, ".")
message("  linked to existing individuals: ", sum(new_rows$origin == "linked-2026"))
message("  new individuals (ids ", min(new_rows$individual_id[new_rows$origin == "new-2026"]),
        "-", max(new_rows$individual_id[new_rows$origin == "new-2026"]), "): ",
        sum(new_rows$origin == "new-2026"))
message("  crosswalk now ", nrow(updated), " rows, ",
        n_distinct(updated$individual_id), " individuals, 1928-", max(updated$year))
