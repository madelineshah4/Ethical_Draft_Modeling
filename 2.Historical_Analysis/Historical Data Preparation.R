library(tidyverse)
library(progressr)
library(tidycensus)
library(wru)

handlers(global = TRUE)
handlers("cli")

setwd("X") # change this to set the workign directory to where this file is located

drafted_stats        <- read_csv("../1.Data_Collection_Prep/clean_final_datasets/drafted_stats.csv")
player_master        <- read_csv("../1.Data_Collection_Prep/clean_final_datasets/player_master.csv")
college_performance  <- read_csv("../1.Data_Collection_Prep/clean_final_datasets/college_performance_history.csv")

#safe max helper to avoid -Inf on all-NA columns
safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

#collapse college_performance to one row per player
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

#join college pieces together
college_clean <- counting_stats %>%
  left_join(rate_stats, by = "athlete_id") %>%
  left_join(college_context, by = "athlete_id")

#collapse drafted_stats to one row per player, filter to 2011+
drafted_clean <- drafted_stats %>%
  group_by(athlete_id) %>%
  summarise(
    player_name    = first(player_name),
    team           = first(team),
    position       = first(position),
    height         = first(height),
    weight         = first(weight),
    draft_position = first(draft_position),
    draft_round    = first(draft_round),
    draft_pick     = first(draft_pick),
    draft_year     = min(nfl_season, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(draft_year >= 2011)

#master join with player_master as base
master <- player_master %>%
  select(athlete_id, first_name, last_name, position, height, weight,
         home_city, home_state, home_country, home_latitude,
         home_longitude, home_county_fips, drafted_flag) %>%
  left_join(
    drafted_clean %>% select(athlete_id, draft_round, draft_pick,
                             draft_position, draft_year),
    by = "athlete_id"
  ) %>%
  left_join(college_clean, by = "athlete_id") %>%
  mutate(
    #recompute drafted_flag to reflect 2011+ only
    drafted_flag = if_else(!is.na(draft_round), 1L, 0L),
    #coverage flags
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

#drop unclassified position players
master <- master %>%
  filter(position != "?")

#position-level median imputation for height and weight
master <- master %>%
  group_by(position) %>%
  mutate(
    weight = if_else(is.na(weight), median(weight, na.rm = TRUE), weight),
    height = if_else(is.na(height), median(height, na.rm = TRUE), height)
  ) %>%
  ungroup()

#derive draft year for undrafted players from last college season + 1
last_college_year <- college_performance %>%
  group_by(athlete_id) %>%
  summarise(last_college_year = max(year, na.rm = TRUE), .groups = "drop")

master <- master %>%
  left_join(last_college_year, by = "athlete_id") %>%
  mutate(
    draft_year = if_else(is.na(draft_year), last_college_year + 1L, draft_year)
  ) %>%
  select(-last_college_year)

#pull acs5 census data
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

#get unique state fips from census eligible players
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

#compute derived census variables
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

#join census data onto master
master <- master %>%
  mutate(GEOID = str_pad(as.character(home_county_fips), width = 5, pad = "0")) %>%
  left_join(census_clean, by = "GEOID") %>%
  select(-GEOID)

cat("census join complete\n")
cat("missing median_income after join:", sum(is.na(master$median_income)), "\n")

#run wru race inference using surname only
cat("running wru race inference...\n")

wru_input <- master %>%
  filter(!is.na(last_name)) %>%
  mutate(surname = last_name) %>%
  select(athlete_id, surname)

wru_results <- predict_race(
  voter.file   = wru_input,
  surname.only = TRUE
)

#clean up wru output and join onto master
wru_clean <- wru_results %>%
  select(
    athlete_id,
    prob_white    = pred.whi,
    prob_black    = pred.bla,
    prob_hispanic = pred.his,
    prob_asian    = pred.asi,
    prob_other    = pred.oth
  )

master <- master %>%
  left_join(wru_clean, by = "athlete_id")

cat("wru inference complete\n")
cat("missing prob_black after join:", sum(is.na(master$prob_black)), "\n")


#save master dataset
write_csv(master, "../1.Data_Collection_Prep/clean_final_datasets/master_edited")



