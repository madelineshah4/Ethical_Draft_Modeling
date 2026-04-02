library(tidyverse)
library(progressr)
library(tidycensus)
library(wru)
library(nflreadr)
library(broom)
library(logistf)

handlers(global = TRUE)
handlers("cli")

setwd("~/Downloads")

# load datasets
player_master       <- read_csv("player_master.csv", show_col_types = FALSE)
college_performance <- read_csv("college_performance_history.csv", show_col_types = FALSE)
combine_data        <- load_combine(seasons = 2018:2025)
nfl_players         <- load_players()

# helper functions
safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

assign_pos_group <- function(pos) {
  case_when(
    pos %in% c("QB")                             ~ "QB",
    pos %in% c("RB", "FB")                       ~ "RB",
    pos %in% c("WR", "CB/WR")                    ~ "WR",
    pos %in% c("TE")                             ~ "TE",
    pos %in% c("C", "G", "OG", "OT", "OL")      ~ "OL",
    pos %in% c("DE", "DT", "DL", "EDGE", "NT")  ~ "DL",
    pos %in% c("LB", "ILB", "OLB")              ~ "LB",
    pos %in% c("CB", "S", "SAF", "DB")           ~ "DB",
    pos %in% c("K", "P", "LS")                   ~ "Special",
    TRUE                                          ~ NA_character_
  )
}

assign_conf_tier <- function(conference) {
  case_when(
    conference %in% c("SEC", "Big Ten", "ACC", "Big 12")           ~ "Power 4",
    conference %in% c("American Athletic", "Mountain West",
                      "Conference USA", "Mid-American", "Sun Belt") ~ "Group of 5",
    conference == "FBS Independents"                                ~ "FBS Independent",
    is.na(conference)                                               ~ NA_character_,
    TRUE                                                            ~ "FCS/Other"
  )
}

# college performance aggregation
with_progress({
  p <- progressor(steps = 3)
  
  p(message = "Aggregating counting stats...")
  counting_stats <- college_performance %>%
    group_by(athlete_id) %>%
    summarise(
      passing_completions  = sum(passing_completions, na.rm = TRUE),
      passing_att          = sum(passing_att, na.rm = TRUE),
      passing_yds          = sum(passing_yds, na.rm = TRUE),
      passing_td           = sum(passing_td, na.rm = TRUE),
      passing_int          = sum(passing_int, na.rm = TRUE),
      rushing_car          = sum(rushing_car, na.rm = TRUE),
      rushing_yds          = sum(rushing_yds, na.rm = TRUE),
      rushing_td           = sum(rushing_td, na.rm = TRUE),
      rushing_long         = safe_max(rushing_long),
      receiving_rec        = sum(receiving_rec, na.rm = TRUE),
      receiving_yds        = sum(receiving_yds, na.rm = TRUE),
      receiving_td         = sum(receiving_td, na.rm = TRUE),
      receiving_long       = safe_max(receiving_long),
      fumbles_fum          = sum(fumbles_fum, na.rm = TRUE),
      fumbles_rec          = sum(fumbles_rec, na.rm = TRUE),
      fumbles_lost         = sum(fumbles_lost, na.rm = TRUE),
      defensive_solo       = sum(defensive_solo, na.rm = TRUE),
      defensive_tot        = sum(defensive_tot, na.rm = TRUE),
      defensive_tfl        = sum(defensive_tfl, na.rm = TRUE),
      defensive_sacks      = sum(defensive_sacks, na.rm = TRUE),
      defensive_qb_hur     = sum(defensive_qb_hur, na.rm = TRUE),
      defensive_pd         = sum(defensive_pd, na.rm = TRUE),
      defensive_td         = sum(defensive_td, na.rm = TRUE),
      interceptions_int    = sum(interceptions_int, na.rm = TRUE),
      interceptions_yds    = sum(interceptions_yds, na.rm = TRUE),
      interceptions_td     = sum(interceptions_td, na.rm = TRUE),
      kicking_fgm          = sum(kicking_fgm, na.rm = TRUE),
      kicking_fga          = sum(kicking_fga, na.rm = TRUE),
      kicking_xpa          = sum(kicking_xpa, na.rm = TRUE),
      kicking_xpm          = sum(kicking_xpm, na.rm = TRUE),
      kicking_pts          = sum(kicking_pts, na.rm = TRUE),
      kicking_long         = safe_max(kicking_long),
      kick_returns_no      = sum(kick_returns_no, na.rm = TRUE),
      kick_returns_yds     = sum(kick_returns_yds, na.rm = TRUE),
      kick_returns_td      = sum(kick_returns_td, na.rm = TRUE),
      kick_returns_long    = safe_max(kick_returns_long),
      punting_no           = sum(punting_no, na.rm = TRUE),
      punting_yds          = sum(punting_yds, na.rm = TRUE),
      punting_long         = safe_max(punting_long),
      punting_in_20        = sum(punting_in_20, na.rm = TRUE),
      punting_tb           = sum(punting_tb, na.rm = TRUE),
      punt_returns_no      = sum(punt_returns_no, na.rm = TRUE),
      punt_returns_yds     = sum(punt_returns_yds, na.rm = TRUE),
      punt_returns_td      = sum(punt_returns_td, na.rm = TRUE),
      punt_returns_long    = safe_max(punt_returns_long),
      .groups = "drop"
    )
  
  p(message = "Aggregating rate stats...")
  rate_stats <- college_performance %>%
    group_by(athlete_id) %>%
    summarise(
      passing_pct       = mean(passing_pct, na.rm = TRUE),
      passing_ypa       = mean(passing_ypa, na.rm = TRUE),
      rushing_ypc       = mean(rushing_ypc, na.rm = TRUE),
      receiving_ypr     = mean(receiving_ypr, na.rm = TRUE),
      kicking_pct       = mean(kicking_pct, na.rm = TRUE),
      kick_returns_avg  = mean(kick_returns_avg, na.rm = TRUE),
      punting_ypp       = mean(punting_ypp, na.rm = TRUE),
      punt_returns_avg  = mean(punt_returns_avg, na.rm = TRUE),
      interceptions_avg = mean(interceptions_avg, na.rm = TRUE),
      .groups = "drop"
    )
  
  p(message = "Pulling college context...")
  college_context <- college_performance %>%
    group_by(athlete_id) %>%
    slice_max(year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(athlete_id, team, conference)
})

college_clean <- counting_stats %>%
  left_join(rate_stats, by = "athlete_id") %>%
  left_join(college_context, by = "athlete_id")

# build enriched nfl_players with college data
nfl_players_enriched <- nfl_players %>%
  filter(!is.na(pfr_id)) %>%
  mutate(espn_id_num = as.numeric(espn_id)) %>%
  left_join(college_clean, by = c("espn_id_num" = "athlete_id")) %>%
  select(pfr_id, first_name, last_name, espn_id_num,
         college_name, college_conference, conference, team,
         passing_completions:punt_returns_long,
         passing_pct:interceptions_avg)

# prepare combine as base population
combine_clean <- combine_data %>%
  filter(!is.na(pfr_id)) %>%
  mutate(
    drafted_flag    = if_else(!is.na(draft_team), 1L, 0L),
    full_name_clean = tolower(str_squish(str_replace_all(player_name, "[^a-zA-Z ]", "")))
  )

combine_matched <- combine_clean %>%
  inner_join(nfl_players_enriched, by = "pfr_id")

combine_unmatched <- combine_clean %>%
  anti_join(nfl_players_enriched, by = "pfr_id")

nfl_players_named <- nfl_players_enriched %>%
  mutate(full_name_clean = tolower(str_squish(str_replace_all(
    paste(first_name, last_name), "[^a-zA-Z ]", ""))))

combine_name_matched <- combine_unmatched %>%
  inner_join(nfl_players_named, by = "full_name_clean", suffix = c("", "_nfl")) %>%
  mutate(pfr_id = coalesce(pfr_id, pfr_id_nfl)) %>%
  select(-pfr_id_nfl)

combine_base <- bind_rows(combine_matched, combine_name_matched) %>%
  group_by(pfr_id) %>%
  slice(1) %>%
  ungroup()

cat("total matched players:", nrow(combine_base), "\n")
cat("unmatched:", nrow(combine_clean) - nrow(combine_base), "\n")

# join hometown fips from player_master via name
player_master_fips <- player_master %>%
  mutate(full_name_clean = tolower(str_squish(str_replace_all(
    paste(first_name, last_name), "[^a-zA-Z ]", "")))) %>%
  select(full_name_clean, home_city, home_state, home_country, home_county_fips) %>%
  group_by(full_name_clean) %>%
  slice(1) %>%
  ungroup()

combine_base <- combine_base %>%
  left_join(player_master_fips, by = "full_name_clean")

# convert height to inches and impute missing ht/wt by position
master <- combine_base %>%
  mutate(
    ht_inches = map_dbl(ht, function(x) {
      if (is.na(x)) return(NA_real_)
      parts <- str_split(x, "-")[[1]]
      if (length(parts) < 2) return(NA_real_)
      as.numeric(parts[1]) * 12 + as.numeric(parts[2])
    }),
    wt = as.numeric(wt)
  ) %>%
  group_by(pos) %>%
  mutate(
    ht_inches = if_else(is.na(ht_inches), median(ht_inches, na.rm = TRUE), ht_inches),
    wt        = if_else(is.na(wt), median(wt, na.rm = TRUE), wt)
  ) %>%
  ungroup() %>%
  mutate(
    is_international = case_when(
      is.na(home_country)   ~ FALSE,
      home_country == "USA" ~ FALSE,
      TRUE                  ~ TRUE
    ),
    census_eligible = case_when(
      is_international        ~ FALSE,
      is.na(home_county_fips) ~ FALSE,
      TRUE                    ~ TRUE
    )
  )

# pull acs5 census data
cat("pulling acs5 census data...\n")

acs_vars <- c(
  median_income    = "B19013_001",
  poverty_total    = "B17001_001",
  poverty_below    = "B17001_002",
  pop_total        = "B02001_001",
  pop_white        = "B02001_002",
  pop_black        = "B02001_003",
  pop_hispanic     = "B03003_003",
  edu_total        = "B15003_001",
  edu_bachelors    = "B15003_022",
  edu_masters      = "B15003_023",
  edu_professional = "B15003_024",
  edu_doctorate    = "B15003_025",
  urban_total      = "B08301_001",
  urban_car        = "B08301_002"
)

state_fips_list <- master %>%
  filter(census_eligible) %>%
  mutate(state_fips = str_pad(str_sub(as.character(home_county_fips), 1, 2), 2, pad = "0")) %>%
  filter(state_fips != "00") %>%
  distinct(state_fips) %>%
  pull(state_fips)

with_progress({
  p <- progressor(steps = length(state_fips_list))
  
  census_raw <- map_dfr(state_fips_list, function(st) {
    p(message = paste("pulling census data for state", st))
    tryCatch(
      get_acs(
        geography = "county",
        variables = acs_vars,
        state     = st,
        year      = 2023,
        survey    = "acs5",
        output    = "wide"
      ),
      error = function(e) NULL
    )
  })
})

census_clean <- census_raw %>%
  transmute(
    GEOID,
    median_income   = median_incomeE,
    poverty_rate    = poverty_belowE / poverty_totalE,
    pct_white       = pop_whiteE / pop_totalE,
    pct_black       = pop_blackE / pop_totalE,
    pct_hispanic    = pop_hispanicE / pop_totalE,
    pct_college_edu = (edu_bachelorsE + edu_mastersE + edu_professionalE + edu_doctorateE) / edu_totalE,
    pct_car_commute = urban_carE / urban_totalE
  )

master <- master %>%
  mutate(GEOID = str_pad(as.character(home_county_fips), width = 5, pad = "0")) %>%
  left_join(census_clean, by = "GEOID") %>%
  select(-GEOID)

cat("census join complete\n")
cat("missing median_income:", sum(is.na(master$median_income)), "\n")

# wru race inference
cat("running wru race inference...\n")

wru_input <- master %>%
  filter(!is.na(last_name)) %>%
  mutate(identifier = row_number()) %>%
  select(identifier, surname = last_name)

wru_results <- predict_race(
  voter.file   = wru_input,
  surname.only = TRUE
)

wru_clean <- wru_results %>%
  select(
    identifier,
    prob_white    = pred.whi,
    prob_black    = pred.bla,
    prob_hispanic = pred.his,
    prob_asian    = pred.asi,
    prob_other    = pred.oth
  )

master <- master %>%
  mutate(identifier = row_number()) %>%
  left_join(wru_clean, by = "identifier") %>%
  select(-identifier)

cat("wru inference complete\n")

# add derived variables
master <- master %>%
  mutate(
    pos_group       = assign_pos_group(pos),
    conference_tier = factor(assign_conf_tier(conference),
                             levels = c("Power 4", "FBS Independent",
                                        "Group of 5", "FCS/Other")),
    pct_black_c     = pct_black - mean(pct_black, na.rm = TRUE),
    prob_black_c    = prob_black - mean(prob_black, na.rm = TRUE),
    median_income_c = median_income - mean(median_income, na.rm = TRUE)
  )

# sanity checks
cat("\ndataset shape\n")
cat("total rows:", nrow(master), "\n")
cat("unique pfr_ids:", n_distinct(master$pfr_id), "\n")
cat("duplicate pfr_ids:", sum(duplicated(master$pfr_id)), "\n")
cat("\ndrafted flag distribution\n")
print(table(master$drafted_flag, useNA = "ifany"))
cat("\nposition distribution\n")
print(table(master$pos, useNA = "ifany"))
cat("\nkey column missingness\n")
master %>%
  summarise(
    missing_pos        = sum(is.na(pos)),
    missing_ht         = sum(is.na(ht_inches)),
    missing_wt         = sum(is.na(wt)),
    missing_forty      = sum(is.na(forty)),
    missing_last_name  = sum(is.na(last_name)),
    missing_conference = sum(is.na(conference)),
    missing_income     = sum(is.na(median_income)),
    missing_prob_black = sum(is.na(prob_black))
  ) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  mutate(pct_missing = round(missing_count / nrow(master) * 100, 1)) %>%
  print()

write_csv(master, "master.csv")
cat("master.csv saved\n")

# performance variables per position group
perf_vars <- list(
  QB = c("passing_yds", "passing_td", "passing_int", "passing_pct", "passing_ypa",
         "rushing_yds", "rushing_td"),
  RB = c("rushing_yds", "rushing_td", "rushing_ypc", "rushing_car",
         "receiving_rec", "receiving_yds"),
  WR = c("receiving_rec", "receiving_yds", "receiving_td", "receiving_ypr",
         "receiving_long"),
  TE = c("receiving_rec", "receiving_yds", "receiving_td", "receiving_ypr"),
  OL = c("ht_inches", "wt"),
  DL = c("defensive_sacks", "defensive_tfl", "defensive_tot",
         "defensive_solo", "defensive_qb_hur"),
  LB = c("defensive_tot", "defensive_solo", "defensive_tfl",
         "defensive_sacks", "defensive_pd", "interceptions_int"),
  DB = c("defensive_tot", "defensive_solo", "defensive_pd",
         "interceptions_int", "defensive_tfl")
)

nonperf_vars <- c("conference_tier", "prob_black_c", "pct_black_c",
                  "median_income_c", "poverty_rate", "ht_inches", "wt")

# phase 2: descriptive analysis
cat("\ndraft rate by conference tier\n")
master %>%
  filter(!is.na(conference_tier)) %>%
  group_by(conference_tier) %>%
  summarise(total = n(), drafted = sum(drafted_flag),
            draft_rate = round(drafted / total * 100, 1)) %>%
  arrange(desc(draft_rate)) %>%
  print()

chisq.test(table(master$conference_tier, master$drafted_flag)) %>% print()

cat("\ndraft rate by position\n")
master %>%
  group_by(pos) %>%
  summarise(total = n(), drafted = sum(drafted_flag),
            draft_rate = round(drafted / total * 100, 1)) %>%
  arrange(desc(draft_rate)) %>%
  print(n = 30)

cat("\ndraft rate by pct_black quartile\n")
master %>%
  filter(!is.na(pct_black)) %>%
  mutate(pct_black_quartile = ntile(pct_black, 4)) %>%
  group_by(pct_black_quartile) %>%
  summarise(total = n(), drafted = sum(drafted_flag),
            draft_rate = round(drafted / total * 100, 1),
            mean_pct_black = round(mean(pct_black), 3)) %>%
  print()

cat("\nt-tests\n")
t.test(prob_black ~ drafted_flag, data = master) %>% print()
t.test(pct_black ~ drafted_flag, data = master) %>% print()
t.test(median_income ~ drafted_flag, data = master) %>% print()
t.test(poverty_rate ~ drafted_flag, data = master) %>% print()

phase2_results <- bind_rows(
  master %>%
    filter(!is.na(conference_tier)) %>%
    group_by(conference_tier) %>%
    summarise(total = n(), drafted = sum(drafted_flag),
              draft_rate = round(drafted / total * 100, 1), .groups = "drop") %>%
    mutate(section = "draft rate by conference tier", group = conference_tier) %>%
    select(section, group, total, drafted, draft_rate),
  master %>%
    group_by(pos) %>%
    summarise(total = n(), drafted = sum(drafted_flag),
              draft_rate = round(drafted / total * 100, 1), .groups = "drop") %>%
    mutate(section = "draft rate by position", group = pos) %>%
    select(section, group, total, drafted, draft_rate),
  master %>%
    filter(!is.na(pct_black)) %>%
    mutate(group = paste("pct_black quartile", ntile(pct_black, 4))) %>%
    group_by(group) %>%
    summarise(total = n(), drafted = sum(drafted_flag),
              draft_rate = round(drafted / total * 100, 1), .groups = "drop") %>%
    mutate(section = "draft rate by pct_black quartile") %>%
    select(section, group, total, drafted, draft_rate),
  tibble(
    section    = "t-test results",
    group      = c("prob_black", "pct_black", "median_income", "poverty_rate"),
    total      = NA_real_, drafted = NA_real_, draft_rate = NA_real_
  ) %>%
    mutate(p_value = c(
      t.test(prob_black ~ drafted_flag, data = master)$p.value,
      t.test(pct_black ~ drafted_flag, data = master)$p.value,
      t.test(median_income ~ drafted_flag, data = master)$p.value,
      t.test(poverty_rate ~ drafted_flag, data = master)$p.value
    ))
)

write_csv(phase2_results, "phase2_results.csv")
cat("phase2_results.csv saved\n")

# phase 3: discrimination detection
results <- list()

for (pos in names(perf_vars)) {
  cat("\nrunning models for:", pos, "\n")
  
  pos_data <- master %>% filter(pos_group == pos)
  pvars    <- perf_vars[[pos]]
  npvars   <- if (pos == "OL") c("conference_tier", "prob_black_c", "pct_black_c",
                                 "median_income_c", "poverty_rate") else nonperf_vars
  
  model_data <- pos_data %>%
    select(drafted_flag, draft_year, all_of(pvars), all_of(npvars)) %>%
    drop_na()
  
  cat("sample size:", nrow(model_data), "\n")
  cat("undrafted:", sum(model_data$drafted_flag == 0), "\n")
  
  if (sum(model_data$drafted_flag == 0) < 10) {
    cat("skipping", pos, "- too few undrafted\n"); next
  }
  
  m1 <- tryCatch(
    glm(as.formula(paste("drafted_flag ~",
                         paste(c(pvars, "draft_year"), collapse = " + "))),
        data = model_data, family = binomial),
    error = function(e) NULL)
  
  m2 <- tryCatch(
    glm(as.formula(paste("drafted_flag ~",
                         paste(c(pvars, "draft_year", npvars), collapse = " + "))),
        data = model_data, family = binomial),
    error = function(e) NULL)
  
  if (!is.null(m1) & !is.null(m2)) {
    results[[pos]] <- list(
      model1      = tidy(m1, conf.int = TRUE),
      model2      = tidy(m2, conf.int = TRUE),
      n           = nrow(model_data),
      n_drafted   = sum(model_data$drafted_flag),
      n_undrafted = sum(model_data$drafted_flag == 0)
    )
  }
}

nonperf_results <- map_dfr(names(results), function(pos) {
  results[[pos]]$model2 %>%
    filter(term %in% c("prob_black_c", "pct_black_c", "median_income_c",
                       "poverty_rate", "ht_inches", "wt") |
             str_detect(term, "conference_tier")) %>%
    mutate(pos_group = pos, n = results[[pos]]$n,
           n_undrafted = results[[pos]]$n_undrafted, significant = p.value < 0.05)
})

perf_results <- map_dfr(names(results), function(pos) {
  results[[pos]]$model1 %>%
    filter(term != "(Intercept)") %>%
    mutate(pos_group = pos, n = results[[pos]]$n,
           n_undrafted = results[[pos]]$n_undrafted, significant = p.value < 0.05)
})

cat("\nnon-performance coefficients\n")
nonperf_results %>%
  select(pos_group, term, estimate, std.error, p.value, significant, n, n_undrafted) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(term, pos_group) %>%
  print(n = 100)

cat("\nsignificant non-performance effects\n")
nonperf_results %>%
  filter(significant) %>%
  select(pos_group, term, estimate, p.value, n, n_undrafted) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(p.value) %>%
  print()

nonperf_results %>% mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("phase3_nonperf_results.csv")
perf_results %>% mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("phase3_perf_results.csv")
nonperf_results %>% filter(significant) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("phase3_significant_findings.csv")

cat("phase 3 results saved\n")

# phase 4: intersectionality
intersection_results <- list()

for (pos in names(perf_vars)) {
  cat("\nrunning intersectionality models for:", pos, "\n")
  
  pos_data <- master %>% filter(pos_group == pos)
  pvars    <- perf_vars[[pos]]
  npvars   <- if (pos == "OL") c("conference_tier", "prob_black_c", "pct_black_c",
                                 "median_income_c", "poverty_rate") else nonperf_vars
  
  model_data <- pos_data %>%
    select(drafted_flag, draft_year, all_of(pvars), all_of(npvars)) %>%
    drop_na()
  
  if (sum(model_data$drafted_flag == 0) < 10) next
  
  m_interaction <- tryCatch(
    glm(as.formula(paste("drafted_flag ~",
                         paste(c(pvars, "draft_year", npvars,
                                 "pct_black_c:conference_tier",
                                 "pct_black_c:prob_black_c",
                                 "pct_black_c:median_income_c"), collapse = " + "))),
        data = model_data, family = binomial),
    error = function(e) NULL)
  
  if (!is.null(m_interaction)) {
    intersection_results[[pos]] <- list(
      model       = tidy(m_interaction, conf.int = TRUE),
      n           = nrow(model_data),
      n_undrafted = sum(model_data$drafted_flag == 0)
    )
  }
}

interaction_coefs <- map_dfr(names(intersection_results), function(pos) {
  intersection_results[[pos]]$model %>%
    filter(str_detect(term, ":")) %>%
    mutate(pos_group = pos, n = intersection_results[[pos]]$n,
           n_undrafted = intersection_results[[pos]]$n_undrafted,
           significant = p.value < 0.05)
})

cat("\ninteraction coefficients\n")
interaction_coefs %>%
  select(pos_group, term, estimate, std.error, p.value, significant, n, n_undrafted) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(term, pos_group) %>%
  print(n = 100)

cat("\nsignificant interactions\n")
interaction_coefs %>%
  filter(significant) %>%
  select(pos_group, term, estimate, p.value, n, n_undrafted) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(p.value) %>%
  print()

interaction_coefs %>% mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("phase4_interaction_results.csv")
interaction_coefs %>% filter(significant) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("phase4_significant_interactions.csv")

cat("phase 4 results saved\n")

# draft capital analysis
cat("\ndraft capital analysis\n")

drafted_only <- master %>% filter(drafted_flag == 1, !is.na(draft_ovr))
cat("drafted players with pick data:", nrow(drafted_only), "\n")

draft_capital_results <- list()

for (pos in names(perf_vars)) {
  cat("\nrunning draft capital model for:", pos, "\n")
  
  pos_data <- drafted_only %>% filter(pos_group == pos)
  pvars    <- perf_vars[[pos]]
  npvars   <- if (pos == "OL") c("conference_tier", "prob_black_c", "pct_black_c",
                                 "median_income_c", "poverty_rate") else nonperf_vars
  
  model_data <- pos_data %>%
    select(draft_ovr, draft_year, all_of(pvars), all_of(npvars)) %>%
    drop_na()
  
  cat("sample size:", nrow(model_data), "\n")
  if (nrow(model_data) < 20) { cat("skipping", pos, "\n"); next }
  
  m1 <- tryCatch(
    lm(as.formula(paste("draft_ovr ~",
                        paste(c(pvars, "draft_year"), collapse = " + "))),
       data = model_data),
    error = function(e) NULL)
  
  m2 <- tryCatch(
    lm(as.formula(paste("draft_ovr ~",
                        paste(c(pvars, "draft_year", npvars), collapse = " + "))),
       data = model_data),
    error = function(e) NULL)
  
  if (!is.null(m1) & !is.null(m2)) {
    draft_capital_results[[pos]] <- list(
      model1 = tidy(m1, conf.int = TRUE),
      model2 = tidy(m2, conf.int = TRUE),
      n      = nrow(model_data)
    )
  }
}

draft_capital_nonperf <- map_dfr(names(draft_capital_results), function(pos) {
  draft_capital_results[[pos]]$model2 %>%
    filter(term %in% c("prob_black_c", "pct_black_c", "median_income_c",
                       "poverty_rate", "ht_inches", "wt") |
             str_detect(term, "conference_tier")) %>%
    mutate(pos_group = pos, n = draft_capital_results[[pos]]$n,
           significant = p.value < 0.05)
})

cat("\ndraft capital: significant non-performance effects\n")
draft_capital_nonperf %>%
  filter(significant) %>%
  select(pos_group, term, estimate, p.value, n) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(p.value) %>%
  print()

draft_capital_nonperf %>% mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("draft_capital_results.csv")
draft_capital_nonperf %>% filter(significant) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("draft_capital_significant.csv")

# combine invitation analysis
cat("\ncombine invitation analysis\n")

perf_vars_combine        <- perf_vars
perf_vars_combine[["OL"]] <- c("height", "weight")

nfl_players_base <- nfl_players %>%
  filter(!is.na(pfr_id), rookie_season >= 2018, rookie_season <= 2025) %>%
  mutate(
    espn_id_num     = as.numeric(espn_id),
    full_name_clean = tolower(str_squish(str_replace_all(
      paste(first_name, last_name), "[^a-zA-Z ]", "")))
  )

combine_pfr_ids <- combine_data %>%
  filter(!is.na(pfr_id)) %>%
  pull(pfr_id) %>%
  unique()

nfl_players_base <- nfl_players_base %>%
  mutate(combine_invited = if_else(pfr_id %in% combine_pfr_ids, 1L, 0L))

cat("combine invited:", sum(nfl_players_base$combine_invited), "\n")
cat("not invited:", sum(nfl_players_base$combine_invited == 0), "\n")

college_context_ci <- college_performance %>%
  group_by(athlete_id) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(athlete_id, team, conference)

college_stats_ci <- college_performance %>%
  group_by(athlete_id) %>%
  summarise(
    passing_yds       = sum(passing_yds, na.rm = TRUE),
    passing_td        = sum(passing_td, na.rm = TRUE),
    passing_int       = sum(passing_int, na.rm = TRUE),
    passing_pct       = mean(passing_pct, na.rm = TRUE),
    passing_ypa       = mean(passing_ypa, na.rm = TRUE),
    rushing_yds       = sum(rushing_yds, na.rm = TRUE),
    rushing_td        = sum(rushing_td, na.rm = TRUE),
    rushing_ypc       = mean(rushing_ypc, na.rm = TRUE),
    rushing_car       = sum(rushing_car, na.rm = TRUE),
    receiving_rec     = sum(receiving_rec, na.rm = TRUE),
    receiving_yds     = sum(receiving_yds, na.rm = TRUE),
    receiving_td      = sum(receiving_td, na.rm = TRUE),
    receiving_ypr     = mean(receiving_ypr, na.rm = TRUE),
    receiving_long    = safe_max(receiving_long),
    defensive_sacks   = sum(defensive_sacks, na.rm = TRUE),
    defensive_tfl     = sum(defensive_tfl, na.rm = TRUE),
    defensive_tot     = sum(defensive_tot, na.rm = TRUE),
    defensive_solo    = sum(defensive_solo, na.rm = TRUE),
    defensive_qb_hur  = sum(defensive_qb_hur, na.rm = TRUE),
    defensive_pd      = sum(defensive_pd, na.rm = TRUE),
    interceptions_int = sum(interceptions_int, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(college_context_ci, by = "athlete_id")

nfl_players_base <- nfl_players_base %>%
  left_join(college_stats_ci, by = c("espn_id_num" = "athlete_id")) %>%
  left_join(player_master_fips, by = "full_name_clean")

census_vars <- master %>%
  select(home_county_fips, median_income, poverty_rate, pct_white,
         pct_black, pct_hispanic, pct_college_edu, pct_car_commute) %>%
  distinct(home_county_fips, .keep_all = TRUE)

nfl_players_base <- nfl_players_base %>%
  left_join(census_vars, by = "home_county_fips") %>%
  mutate(
    pos_group       = assign_pos_group(position),
    conference_tier = factor(assign_conf_tier(conference),
                             levels = c("Power 4", "FBS Independent",
                                        "Group of 5", "FCS/Other")),
    pct_black_c     = pct_black - mean(pct_black, na.rm = TRUE),
    median_income_c = median_income - mean(median_income, na.rm = TRUE)
  )

combine_results <- list()

for (pos in names(perf_vars_combine)) {
  cat("\nrunning combine invitation model for:", pos, "\n")
  
  pos_data <- nfl_players_base %>% filter(pos_group == pos)
  pvars    <- perf_vars_combine[[pos]]
  npvars   <- if (pos == "OL") c("conference_tier", "pct_black_c", "median_income_c",
                                 "poverty_rate") else
                                   c("conference_tier", "pct_black_c", "median_income_c",
                                     "poverty_rate", "height", "weight")
  
  model_data <- pos_data %>%
    select(combine_invited, rookie_season, all_of(pvars), all_of(npvars)) %>%
    drop_na()
  
  cat("sample size:", nrow(model_data), "\n")
  cat("invited:", sum(model_data$combine_invited), "\n")
  cat("not invited:", sum(model_data$combine_invited == 0), "\n")
  
  if (sum(model_data$combine_invited == 0) < 10 |
      sum(model_data$combine_invited) < 10) {
    cat("skipping", pos, "\n"); next
  }
  
  m1 <- tryCatch(
    logistf(as.formula(paste("combine_invited ~",
                             paste(c(pvars, "rookie_season"), collapse = " + "))),
            data = model_data),
    error = function(e) NULL)
  
  m2 <- tryCatch(
    logistf(as.formula(paste("combine_invited ~",
                             paste(c(pvars, "rookie_season", npvars), collapse = " + "))),
            data = model_data),
    error = function(e) NULL)
  
  if (!is.null(m1) & !is.null(m2)) {
    combine_results[[pos]] <- list(
      model1      = tibble(term = names(m1$coefficients), estimate = m1$coefficients,
                           std.error = sqrt(diag(vcov(m1))), p.value = m1$prob),
      model2      = tibble(term = names(m2$coefficients), estimate = m2$coefficients,
                           std.error = sqrt(diag(vcov(m2))), p.value = m2$prob),
      n           = nrow(model_data),
      n_invited   = sum(model_data$combine_invited),
      n_uninvited = sum(model_data$combine_invited == 0)
    )
  }
}

combine_nonperf <- map_dfr(names(combine_results), function(pos) {
  combine_results[[pos]]$model2 %>%
    filter(term %in% c("pct_black_c", "median_income_c", "poverty_rate",
                       "height", "weight") |
             str_detect(term, "conference_tier")) %>%
    mutate(pos_group = pos, n = combine_results[[pos]]$n,
           n_uninvited = combine_results[[pos]]$n_uninvited,
           significant = p.value < 0.05)
})

cat("\ncombine invitation: significant non-performance effects\n")
combine_nonperf %>%
  filter(significant) %>%
  select(pos_group, term, estimate, p.value, n, n_uninvited) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  arrange(p.value) %>%
  print()

combine_nonperf %>% mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("combine_invitation_results.csv")
combine_nonperf %>% filter(significant) %>%
  mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
  write_csv("combine_invitation_significant.csv")

cat("\nall results saved to ~/Downloads\n")