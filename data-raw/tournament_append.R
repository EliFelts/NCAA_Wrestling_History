library(tidyverse)
library(readxl)


# The seeds sheet Name column is "Last, First". The comma is an explicit
# delimiter, so parsing BOTH parts off it resolves multi-word names with no
# heuristics: "Van Dee, Jacob" -> last "Van Dee"; "Smith, Lee Roy" -> first
# "Lee Roy". (str_remove keeps everything after the first comma, so a stray
# second comma like "O'Connor, John Jr." still parses.)
parse_seed_names.f <- function(df) {
  df %>%
    mutate(
      last_name  = str_squish(word(Name, 1, sep = ",")),
      first_name = str_squish(str_remove(Name, "^[^,]*,")),
      full_name  = str_c(first_name, last_name, sep = " "),
      first_last = str_c(first_name, last_name, sep = "_"),
      team2      = str_c("(", Team, ")"),
      wrestler   = str_c(full_name, team2, sep = " ")
    )
}

seed_read.f <- function(filename) {
  read_excel(filename) %>%
    parse_seed_names.f() %>%
    select(wrestler, seed = Seed, record = Record)
}

seeds.sheet <- "data-raw/2026/ncaaseeds_2026.xlsx"

seeds <- read_excel(seeds.sheet) %>%
  parse_seed_names.f() %>%
  mutate(
    wins_start = as.numeric(word(Record, 1, sep = "-")),
    losses_start = as.numeric(word(Record, 2, sep = "-"))
  )

# Crosswalk for the results sheet, where names arrive as "First Last (Team)"
# with no comma to disambiguate the split. Join on this to inherit the
# authoritative first/last split from the seeds; anything left unmatched with a
# multi-token name is printed for a one-time manual check (usually empty --
# multi-word-name wrestlers are seeded).
name_xwalk <- seeds %>%
  distinct(full_name, team = Team, first_name, last_name, first_last)

# participant_start_adjusted <- participant_start25 %>%
#   select(-any_of(c("first_name", "last_name", "first_last"))) %>%
#   left_join(name_xwalk, by = c("full_name", "team"))
#
# participant_start_adjusted %>%
#   filter(is.na(first_last), str_count(full_name, " ") >= 2) %>%
#   distinct(full_name, team) %>%
#   print(n = Inf)

# ---------------------------------------------------------------------------
# Match results
# ---------------------------------------------------------------------------

tourney_year <- 2026
results_file <- "data-raw/2026/ncaa_results_2026.xlsx"

# How a trackwrestling result phrase ("... won <result_type> over ...") maps to
# a scored outcome. To handle a new phrasing, append a row here -- read_round.f()
# warns with the raw line whenever it meets a result_type that isn't listed,
# so unrecognized results announce themselves instead of turning up as NA.
result_key <- tibble(
  result_type = c(" by decision ", " in tie breaker - 1 ", " by major decision ",
                  " by fall ", " in sudden victory - 1 ", " by tech fall ",
                  " by injury default ", " by medical forfeit ",
                  " in tie breaker - 2 ", " in TB-2 by riding time ",
                  " by forfeit ", " in sudden victory - 2 ",
                  " in TB-2 by fall ", " in the ultimate tie breaker ",
                  " by disqualification ", " in SV-1 by fall ",
                  " in tie breaker - 3 ", " in SV-2 by fall ",
                  " in double overtime ", " in TB-3 by riding time "),
  result = c("Decision", "Decision", "Major Decision",
             "Fall", "Decision", "Technical Fall",
             "Injury Default", "Medical Forfeit",
             "Decision", "Decision",
             "Medical Forfeit", "Decision",
             "Fall", "Decision",
             "Disqualification", "Fall",
             "Decision", "Fall",
             "Decision", "Decision"),
  ot = c(F, T, F, F, T, F, F, F, T, T, F, T, T, T, F, T, T, T, T, T),
  bonus = c(0, 0, 1, 2, 0, 1.5, 2, 2, 0, 0, 2, 0, 2, 0, 2, 2, 0, 2, 0, 0)
)

# Template with one row per bout: bout number plus advancement / placement point
# values. Row order within a weight encodes bracket position, so numbering the
# bouts *within each weight* (bout_in_weight) lets a round join positionally per
# weight -- a weight missing a match in some round no longer shifts the rest.
bout.dat <- read_excel("data-raw/bout_template.xlsx", sheet = "tournament_all") %>%
  group_by(round, weight_class) %>%
  mutate(bout_in_weight = row_number()) %>%
  ungroup()

# Parse one pasted trackwrestling round sheet into scored bouts.
#   sheet       - sheet name inside results_file
#   round_label - override the round name parsed from the text. Needed only for
#                 the consolation prelim, which trackwrestling also prints as
#                 "Prelim"; without the override it joins the championship
#                 Prelim template rows and picks up 1.0 advancement pts, not 0.5.
read_round.f <- function(sheet, round_label = NULL) {
  parsed <- read_excel(results_file, sheet = sheet) %>%
    rename(combo = 1) %>%
    filter(!is.na(combo)) %>%
    mutate(
      # weight-class marker rows ("133", "141", ...) sit just before that
      # weight's bouts; carry them down, default the leading block to 125
      is_label = str_detect(combo, "^\\s*\\d{2,3}\\s*$"),
      weight_class = as.numeric(if_else(is_label, str_trim(combo), NA_character_))
    ) %>%
    fill(weight_class, .direction = "down") %>%
    mutate(weight_class = replace_na(weight_class, 125)) %>%
    filter(!is_label) %>%
    mutate(
      round = if (is.null(round_label)) str_squish(word(combo, 1, sep = "-")) else round_label,
      result_type = str_extract(combo, "(?<=\\bwon\\b)(.*?)(?=\\bover\\b)"),
      # split on the literal " over " separator -- NOT "(?<=over)", which also
      # fires inside "double overtime"
      over_tail = str_extract(combo, "(?<= over ).*"),
      winner = str_squish(str_extract(str_extract(combo, "(?<=-)(.*?)(?=won)"),
                                      "^([^()]*\\([^()]*\\))")),
      loser  = str_squish(str_extract(over_tail, "^([^()]*\\([^()]*\\))")),
      # trailing parenthetical of the tail, tolerating nesting so a tech fall's
      # "(TF-1.5 5:13 (20-4))" comes through whole; anchored at $ so the loser's
      # "(Team)" can't be picked up instead
      result_paren = str_extract(str_trim(over_tail),
                                 "\\([^()]*(?:\\([^()]*\\)[^()]*)*\\)$"),
      result_description = str_trim(str_sub(result_paren, 2, -2))
    ) %>%
    filter(!is.na(result_type)) %>%
    left_join(result_key, by = "result_type")

  unknown <- parsed %>% filter(is.na(result)) %>% distinct(result_type, combo)
  if (nrow(unknown) > 0) {
    warning("[", sheet, "] result_type missing from result_key -- add these rows:\n",
            paste0("  '", unknown$result_type, "'  <-  ", unknown$combo, collapse = "\n"),
            call. = FALSE)
  }

  parsed %>%
    mutate(
      # score "W-L" is the first digit-dash-digit in the description: covers
      # decisions / majors / OT scores and the "(19-3)" inside a tech fall, and
      # is naturally NA for falls / forfeits / DQ (times carry a colon, not a dash)
      match_points = str_extract(result_description, "\\d+\\s*-\\s*\\d+"),
      winner_match_points = as.numeric(str_squish(word(match_points, 1, sep = "-"))),
      loser_match_points  = as.numeric(str_squish(word(match_points, 2, sep = "-"))),
      termination_time = if_else(
        result %in% c("Fall", "Injury Default", "Technical Fall"),
        str_extract(result_description, "\\d+:\\d+"),
        NA_character_
      )
    ) %>%
    group_by(round, weight_class) %>%
    mutate(bout_in_weight = row_number()) %>%
    ungroup() %>%
    left_join(bout.dat, by = c("round", "weight_class", "bout_in_weight")) %>%
    rowwise() %>%
    mutate(winner_team_points_secured = sum(advancement_value,
                                            secured_placement_points, bonus)) %>%
    ungroup() %>%
    select(bout, round, weight_class, winner, loser, result,
           winner_match_points, loser_match_points, termination_time, ot,
           advancement_value, secured_placement_points, bonus,
           winner_team_points_secured)
}

match.bind <- bind_rows(
  read_round.f("pigtail"),
  read_round.f("round1"),
  read_round.f("consprelim", round_label = "Consolation Prelim"),
  read_round.f("round2"),
  read_round.f("cons1"),
  read_round.f("quarters"),
  read_round.f("cons2"),
  read_round.f("cons3"),
  read_round.f("cons4"),
  read_round.f("cons5"),
  read_round.f("cons6"),
  read_round.f("semis"),
  read_round.f("placement"),
  read_round.f("finals")
) %>%
  mutate(year = tourney_year)

# summarize placement

place.key2 <- tibble(final_win=c("1st Place Match","Semifinal","3rd Place Match",
                                 "Cons. Semi","5th Place Match","Cons. Round 5",
                                 "7th Place Match","Cons. Round 4"),
                     placement=c("First","Second","Third",
                                 "Fourth","Fifth","Sixth",
                                 "Seventh","Eigth"),
                     place=c(1:8))


place_match.key=tibble(round=c("1st Place Match","3rd Place Match",
                               "5th Place Match","7th Place Match"),
                       winner_place=c("First","Third","Fifth","Seventh"),
                       loser_place=c("Second","Fourth","Sixth","Eighth"))


aa.placement_winners <- match.bind %>%
  inner_join(place_match.key,by="round") %>%
  select(wrestler=winner,
         placement=winner_place)

aa.placement_losers <- match.bind %>%
  inner_join(place_match.key,by="round") %>%
  select(wrestler=loser,
         placement=loser_place)


aa.placement <- bind_rows(aa.placement_winners,
                          aa.placement_losers)

# change the record stuff to wins and losses in separate
# columns - will fix the date autocorrect and work
# better for summarization; might as well work out
# the logic to tabulate wins and losses in the tourney
# as well to get final records; and, add columns
# distinguishing advancement and bonus points

participants <- seeds |>
  select(wrestler,weight_class,first_name,last_name,team=Team,
         first_last,seed=Seed,record=Record)

wrestler_scores <- match.bind %>%
  group_by(winner) %>%
  summarize(team_points=sum(winner_team_points_secured),
            bonus_points=sum(bonus)) %>%
  rename(wrestler=winner) %>%
  left_join(aa.placement,by="wrestler") %>%
  right_join(participants,by="wrestler") %>%
  mutate(team_points=ifelse(is.na(team_points),0,
                            team_points)) %>%
  select(wrestler,weight_class,first_name,last_name,team,
         first_last,seed,record_starting=record,placement,
         team_points,bonus_points)


# wrestler win-loss in tournament?

wrestler_win <- match.bind %>%
  group_by(winner) %>%
  summarize(wins=n()) %>%
  rename(wrestler=winner)

wrestler_lose <- match.bind %>%
  group_by(loser) %>%
  summarize(losses=n()) %>%
  rename(wrestler=loser)


wrestler_scores.join <- wrestler_scores %>%
  left_join(wrestler_win,by="wrestler") %>%
  left_join(wrestler_lose,by="wrestler") %>%
  mutate(wins_ncaa=ifelse(is.na(wins),0,wins),
         losses_ncaa=ifelse(is.na(losses),0,losses),
         wins_start=as.numeric(word(record_starting,1,sep="-")),
         losses_start=as.numeric(word(record_starting,2,sep="-")),
         wins_final=wins_start+wins_ncaa,
         losses_final=losses_start+losses_ncaa) %>%
  select(wrestler,weight_class,first_name,last_name,
         team,first_last,seed,placement,team_points,
         bonus_points,wins_ncaa,losses_ncaa,
         wins_start,losses_start,wins_final,
         losses_final) %>%
  mutate(year=2026)

