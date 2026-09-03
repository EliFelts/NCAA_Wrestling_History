# Wrestler identity

Deciding which tournament appearances belong to the **same person**, across
transfers, weight changes, redshirt gaps, name collisions, and nickname swaps.

## The idea

`data/wrestlers.rds` is trusted through 2025. Its `wrestler_id`
(`"First Last_startYear-endYear"`, `_1`/`_2` on the rare same-name same-range
pair) already encodes every manual same-name split from the original build.

We freeze that into a **stable `individual_id`** — an integer that never moves
when a wrestler comes back for another year. The app's
`First Last_startYear-endYear` label becomes a *derived* display string
(`min`/`max` year per `individual_id`), rebuilt each run, not a stored key. So
adding a year is: assign `individual_id` to ~330 new wrestler-seasons, append.
No historical rows change; year ranges "just grow".

## Files

| file | what |
|---|---|
| `seed_crosswalk.R` | one-time: `data/wrestlers.rds` → `data-raw/wrestler_identity.csv` (one row per wrestler-season, stable `individual_id`). Run once; `FORCE_RESEED=1` to rebuild. Applies `id_corrections.csv` on the way out. |
| `id_corrections.csv` | fixes to the frozen `<=2025` split: `data/wrestlers.rds` merged some same-name people who are actually distinct. One row per `(match_name, match_team, match_year)` → `set_individual_id`. Use a fresh id above the seed max to split one person out, or an existing id to merge two. |
| `fn_identity.R` | `classify_year()` (split new seasons into linked / new / review) and `resolve_review()` (fold hand decisions back in). Thresholds in `identity_thresholds`. |
| `classify_year.R` | runner: pulls the new year off `data-raw/tournament_append.R`, classifies, writes the two CSVs below. |
| `check_review.R` | dry-runs a hand-edited `identity_review.csv` — flags bad ids / unresolved rows, prints what would be appended. |
| `append_year.R` | folds `identity_auto.csv` + the resolved `identity_review.csv` into `wrestler_identity.csv` (one row per wrestler-season, ids minted for new people). Refuses if the year is already there. |
| `build_wrestlers.R` | rebuilds `data/wrestlers.rds` (+`individual_id`, derived `wrestler_id`) and re-stamps `data/matches.rds` (`winner_individual_id` / `loser_individual_id`) from the crosswalk. Counting stats carried through untouched. |
| `../wrestler_identity.csv` | the crosswalk. System of record for "who is a distinct person". |
| `../<year>/identity_auto.csv` | linked + new, auto-decided. Scan the `flag` column (transfers, multi-class moves, 2+ yr gaps). |
| `../<year>/identity_review.csv` | the judgment calls. Fill `decision_individual_id` (link) **or** `decision_is_new = TRUE` (new person) per row. |

## How a row is classified

For each new-year wrestler-season, match by exact `"First Last"` against every
individual seen in a prior year:

- **new** — no prior individual with this name.
- **linked** — exactly one prior individual, and it looks continuous: gap ≤ 3
  yrs, first→now span ≤ 7 yrs, weight move ≤ 2 classes. Team change alone is
  fine (transfers are normal) and just gets flagged.
- **review** — everything else:
  - name shared by 2+ prior individuals
  - 4+ yr gap, or span past the eligibility ceiling
  - 3+ weight-class move
  - punctuation/spacing variant of a prior name (`O\`Dell` vs `O'Dell`)
  - same last name + first initial as a prior individual active in the last 3
    yrs — a nickname swap (`Joey`/`Joseph`) or a sibling on the circuit
    (`Beau` vs `Brock Mantanona`)

2026 dry run: **184 linked, 131 new, 15 review**.

## Yearly flow

```r
Rscript data-raw/identity/classify_year.R     # -> identity_auto.csv + identity_review.csv
#   edit data-raw/2026/identity_review.csv by hand, one decision per wrestler:
#     decision_individual_id = <candidate_ids value>   to link, OR
#     decision_is_new = TRUE                            for a new person
#   multi-candidate wrestler (one row per candidate): decide the right row,
#     leave the siblings blank
#   found an OLD merged homonym? add a row to id_corrections.csv, FORCE_RESEED,
#     re-run classify_year.R
Rscript data-raw/identity/check_review.R       # validate the edits
Rscript data-raw/identity/append_year.R        # grow wrestler_identity.csv
Rscript data-raw/identity/fold_year.R          # add the year's match/wrestler/seed ROWS
Rscript data-raw/identity/build_wrestlers.R    # key them: individual_id + derived wrestler_id
Rscript data-raw/01_read_reference_xlsx.R      # (once the year's official standings are in team_scores.xlsx)
Rscript data-raw/02_prep_app_data.R            # -> data/app_tables.rds
```

`fold_year.R` reads the year straight off `data-raw/tournament_append.R`
(`match.bind`, `wrestler_scores.join`), maps team names through `R/fn_teams.R`,
and appends rows in the exact `matches.rds` / `wrestlers.rds` / `seeds.rds`
schema — `total_match_points` / `margin` / `margin2`, `falls` / `techs` /
`terminations` / `bonus` / `_noprelim`, `falls_time` / `tech_time`, official
`advancement_value` (NA on pigtails, matching every year since 2013). Then
`build_wrestlers.R` (idempotent — safe to re-run) keys the new rows and widens
the `wrestler_id` range on every returning wrestler.

## 2026 run — done, verified through `data/app_tables.rds`

- crosswalk: 24,441 → **24,771 rows**, 13,349 individuals, 1928–2026 (192 linked, 138 new)
- `matches.rds`: +640 rows (42,517); `winner_individual_id` on 42,378, `loser` on 39,810
- `wrestlers.rds`: +330 rows (24,771); +`individual_id`; 192 returning wrestlers' `wrestler_id` widened to `…-2026`; the James Conway split
- `seeds.rds`: +330 rows (24,604)
- `team_results_annual` 2026: reconstructed `score` (Penn State 181.5) lands on the official standings; `careers_summary` extends Haines/Mesenbrink/Van Dee to 2026; every module UI builds
- **21 non-bye match rows** (all ≤2022) left unkeyed — pre-existing name-string
  mismatches between `matches.rds` and `wrestlers.rds` (`J\`Den`/`J'Den Cox`,
  `Jakob \`Bubba\` Scheffel`, `Requir van der Merwe`, mislabeled 1929 byes, a
  2009 `NA` winner). Original `winner_wrestler_id` string kept; only
  `individual_id` is `NA`.

## Known quirks surfaced by the seed (pre-existing, not fixed)

- 15 `individual_id`s span ≥ 7 calendar years — mostly legit (Steveson
  2019–25, Olympic/mission redshifts); a few old ones may be unsplit homonyms.
- 3 rows in `data/wrestlers.rds` have a season `year` outside their
  `wrestler_id` range (`Tom Best_1999-1992` reversed; `Bob Furlan` carries
  `Bob Funk`'s id).
