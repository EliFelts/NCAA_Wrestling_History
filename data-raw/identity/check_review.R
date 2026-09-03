### Dry-run a hand-edited identity_review.csv: does every wrestler-season have
### exactly one clean decision, and what would resolve_review() append?
###
###   Rscript data-raw/identity/check_review.R [path-to-review.csv]

library(tidyverse)
source("data-raw/identity/fn_identity.R")

YEAR <- 2026
path <- commandArgs(trailingOnly = TRUE)
path <- if (length(path)) path[1] else file.path("data-raw", YEAR, "identity_review.csv")

review <- read_csv(path, show_col_types = FALSE)
crosswalk <- read_csv("data-raw/wrestler_identity.csv", show_col_types = FALSE)

cat("file:", path, "  rows:", nrow(review), "\n\n")

# --- shape checks --------------------------------------------------------
id_raw <- review$decision_individual_id
non_int <- !is.na(id_raw) & (as.character(id_raw) != as.character(suppressWarnings(as.integer(id_raw))))
if (any(non_int)) {
  cat("!! decision_individual_id not a clean integer on", sum(non_int), "row(s):\n")
  print(review$name[non_int]); cat("\n")
}
known <- unique(crosswalk$individual_id)
bad_id <- !is.na(suppressWarnings(as.integer(id_raw))) &
  !(as.integer(id_raw) %in% known)
if (any(bad_id)) {
  cat("!! decision_individual_id not found in the crosswalk on", sum(bad_id), "row(s):\n")
  print(distinct(review[bad_id, c("name", "decision_individual_id")])); cat("\n")
}
review$decision_individual_id <- suppressWarnings(as.integer(id_raw))

# --- the real test: resolve_review() ----------------------------------
add <- tryCatch(resolve_review(review, crosswalk),
                error = function(e) { cat("!! ", conditionMessage(e), "\n"); NULL })

if (!is.null(add)) {
  cat("OK -- ", nrow(add), " wrestler-season(s) would be appended:\n",
      "   linked to an existing individual: ", sum(add$individual_id %in% known), "\n",
      "   new individual_id minted:         ", sum(!add$individual_id %in% known), "\n\n", sep = "")
  print(as.data.frame(add[c("individual_id", "name", "team", "year", "origin")]),
        row.names = FALSE)
}
