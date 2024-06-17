library(tidyverse)
library(nflreadr)


  dic <- dictionary_pbp
  
  teams <- load_teams() %>%
    mutate(team_color = ifelse(team_color == "#D3BC8D", "#857952", team_color)) %>%
    select(team_abbr, team_color)
  
  
  ngs <- load_nextgen_stats(stat_type = c("passing")) %>%
    mutate(week = ifelse(week == 23, 22, week)) %>%
    filter(season == "2023") %>%
    filter(week >= 1) %>%
    arrange(player_last_name) %>%
    mutate(att_avg_time_to_throw = attempts*avg_time_to_throw) %>%
    mutate(att_avg_completed_air_yards = attempts*avg_completed_air_yards) %>%
    mutate(att_avg_intended_air_yards = attempts*avg_intended_air_yards) %>%
    mutate(att_aggressiveness = attempts*aggressiveness) %>%
    mutate(att_avg_air_yards_to_sticks = attempts*avg_air_yards_to_sticks) %>%
    mutate(att_exp_completion_percentage = attempts*expected_completion_percentage) %>%
    mutate(att_completion_percentage_above_expectation = attempts*completion_percentage_above_expectation) %>%
    mutate(team_abbr = ifelse(team_abbr == "LAR", "LA", team_abbr)) %>%
    group_by(player_gsis_id, week) %>%
    summarize(attempts_ngs = sum(attempts),
              att_avg_time_to_throw,
              att_avg_completed_air_yards,
              att_avg_intended_air_yards,
              att_aggressiveness,
              max_completed_air_distance,
              att_avg_air_yards_to_sticks,
              att_exp_completion_percentage,
              att_completion_percentage_above_expectation) 
  
  combine <- load_combine() %>%
    filter(pos == "QB") %>%
    select(player_name, forty, vertical, broad_jump, cone, shuttle)
  
  contracts <- load_contracts() %>%
    filter(position == "QB") %>%
    mutate(guarantee_pct = guaranteed / value) %>%
    group_by(player) %>%
    filter(year_signed == max(year_signed) | (year_signed == max(year_signed) & apy == max(apy))) %>%
    slice_max(order_by = apy) %>%
    ungroup() %>%
    select(player, apy, guaranteed, guarantee_pct, year_signed) %>%
    distinct()
  
  players <- load_players() %>%
    mutate(
           draft_number = as.numeric(draft_number),
           jersey_number = as.numeric(jersey_number),
           weight = as.numeric(weight),
           bmi = (weight / (height^2)) * 703) %>%
  #         birth_month = as.integer(format(as.Date(birth_date), "%m")),
  #         birth_day = as.integer(format(as.Date(birth_date), "%d")),
  #         star_sign = case_when(
  #           (birth_month == 3 & birth_day >= 21) | (birth_month == 4 & birth_day <= 19) ~ "Aries",
  #           (birth_month == 4 & birth_day >= 20) | (birth_month == 5 & birth_day <= 20) ~ "Taurus",
  #           (birth_month == 5 & birth_day >= 21) | (birth_month == 6 & birth_day <= 20) ~ "Gemini",
  #           (birth_month == 6 & birth_day >= 21) | (birth_month == 7 & birth_day <= 22) ~ "Cancer",
  #           (birth_month == 7 & birth_day >= 23) | (birth_month == 8 & birth_day <= 22) ~ "Leo",
  #           (birth_month == 8 & birth_day >= 23) | (birth_month == 9 & birth_day <= 22) ~ "Virgo",
  #           (birth_month == 9 & birth_day >= 23) | (birth_month == 10 & birth_day <= 22) ~ "Libra",
  #           (birth_month == 10 & birth_day >= 23) | (birth_month == 11 & birth_day <= 21) ~ "Scorpio",
  #           (birth_month == 11 & birth_day >= 22) | (birth_month == 12 & birth_day <= 21) ~ "Sagittarius",
  #           (birth_month == 12 & birth_day >= 22) | (birth_month == 1 & birth_day <= 19) ~ "Capricorn",
  #           (birth_month == 1 & birth_day >= 20) | (birth_month == 2 & birth_day <= 18) ~ "Aquarius",
  #           (birth_month == 2 & birth_day >= 19) | (birth_month == 3 & birth_day <= 20) ~ "Pisces",
  #           TRUE ~ "Unknown"  # Default case for unknown star signs
  #         )) %>%
    group_by(gsis_id) %>%
    summarize(short_name, display_name, height, weight, draft_number, jersey_number, bmi, years_of_experience)
  
#  player_stats <- load_player_stats(seasons = 2023) %>%
#    filter(position == "QB") %>%
#    mutate(Quarterback = paste(player_display_name, " (", recent_team, ")", sep = "")) %>%
#    group_by(player_id, week) %>%
#    summarize(attempts,
#              fumbles = sum(sack_fumbles + rushing_fumbles, na.rm = TRUE),
#              fumbles_lost = sum(sack_fumbles_lost + rushing_fumbles_lost, na.rm = TRUE),
#              position,
#              player_short_name = player_name,
#              player_display_name,
#              team_abbr = recent_team,
#              Quarterback) %>%
#    left_join(players, by = c('player_id' = 'gsis_id'))
  
  dic <- dictionary_pbp
  
  
    pass_data <- load_pbp() %>%
       filter(!is.na(down)) %>%
       filter(week <= 18) %>%
   #   left_join(players, by = c('passer_player_id' = 'gsis_id')) %>%
    #  mutate(Quarterback = paste(display_name, " (", posteam, ")", sep = "")) %>%
      mutate(home = ifelse(home_team == posteam, 1, 0),
             redzone = ifelse(yardline_100 <= 20, 1, 0),
             garbage = ifelse(wp <= 0.1 | wp >= 0.9, 1, 0)) %>%
      group_by(passer_player_id, week, down, qtr, home, redzone, garbage) %>%
      summarize(#Quarterback = last(Quarterback[!is.na(Quarterback)]),
                #player_short_name = last(short_name[!is.na(short_name)]),
                #player_display_name = last(display_name[!is.na(display_name)]),
                 team_abbr = last(posteam),
                 attempts = sum(complete_pass == 1 | incomplete_pass == 1 | interception == 1, na.rm = T),
                 sack_fumbles = sum(fumble == 1 & fumbled_1_player_id == passer_player_id),
                 sack_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == passer_player_id & fumble_recovery_1_team != posteam),
                 pass_plays = n()) %>%
     # left_join(players, by = c('passer_player_id' = 'gsis_id')) %>%
      filter(!is.na(passer_player_id))
            
    
    
    
    rush_data <- load_pbp() %>%
      filter(!is.na(down)) %>%
      filter(week <= 18) %>%
      mutate(home = ifelse(home_team == posteam, 1, 0),
             redzone = ifelse(yardline_100 <= 20, 1, 0),
             garbage = ifelse(wp <= 0.1 | wp >= 0.9, 1, 0)) %>%
    #  left_join(players, by = c('rusher_player_id' = 'gsis_id')) %>%
      group_by(rusher_player_id, week, down, qtr, home, redzone, garbage) %>%
      summarize(rush_fumbles = sum(fumble == 1 & fumbled_1_player_id == rusher_player_id),
                rush_fumbles_lost = sum(fumble_lost == 1 & fumbled_1_player_id == rusher_player_id & fumble_recovery_1_team != posteam),
                rush_plays = n()) 
          
    
    
  data <- load_pbp() %>%
    filter(!is.na(down), !is.na(epa), pass+rush == 1) %>%
    filter(week <= 18) %>%
    mutate(id = ifelse(is.na(id), passer_player_id, id)) %>%
    separate(drive_time_of_possession, into = c("drive_minutes", "drive_seconds"), sep = ":") %>%
    mutate(drive_minutes = as.numeric(drive_minutes), drive_seconds = as.numeric(drive_seconds), 
           drive_possession_seconds = drive_minutes * 60 + drive_seconds) %>%
    mutate(home = ifelse(home_team == posteam, 1, 0),
           redzone = ifelse(yardline_100 <= 20, 1, 0),
           garbage = ifelse(wp <= 0.1 | wp >= 0.9, 1, 0)) %>%
    left_join(pass_data, by = c("id" = "passer_player_id", "week", "down", "qtr", "home", "redzone", "garbage")) %>%
    left_join(rush_data, by = c("id" = "rusher_player_id", "week", "down", "qtr", "home", "redzone", "garbage")) %>%
    left_join(teams, by = c('posteam' = 'team_abbr')) %>%
    group_by(id, week, down, qtr, home, redzone, garbage) %>%
    reframe(#  player_short_name = last(short_name[!is.na(short_name)]),
             # player_display_name = last(display_name[!is.na(display_name)]),
             # height = last(height),
            #  draft_number = last(draft_number),
            #  jersey_number = last(jersey_number),
            #  bmi = last(bmi),
            #  weight = last(weight),
            #  years_of_experience = max(as.numeric(years_of_experience)),
              team_abbr = last(posteam),
           #   Quarterback = last(Quarterback),
              data_attempts = n(),
              attempts = last(attempts),
              rush_attempts = sum(rush + qb_scramble, na.rm = TRUE),
              cp_attempts = sum(ifelse(is.na(cp),0,1)),
              success = sum(ifelse(qb_epa > 0, 1, 0)),
              qb_epa = sum(qb_epa, na.rm = TRUE),
              cp = sum(cp, na.rm = TRUE),
              cpoe = sum(cpoe, na.rm = TRUE),
              yards_after_catch = sum(yards_after_catch, na.rm = TRUE),
              air_epa = sum(air_epa, na.rm = TRUE),
              no_huddle = sum(no_huddle, na.rm = TRUE),
              qb_scrambles = sum(qb_scramble, na.rm = TRUE),
              passing_yards = sum(passing_yards, na.rm = TRUE),
              rushing_yards = sum(rushing_yards, na.rm = TRUE),
              pass_touchdown = sum(pass_touchdown, na.rm = TRUE),
              rush_touchdown = sum(rush_touchdown, na.rm = TRUE),
              interception = sum(interception, na.rm = TRUE),
              completions = sum(complete_pass, na.rm = TRUE),
              incomplete_pass = sum(incomplete_pass, na.rm = TRUE), 
              sacks = sum(sack, na.rm = TRUE),
              tot_sack_yards = sum(-yards_gained[sack == 1], na.rm = TRUE),
              tot_scramble_yards = sum(yards_gained[qb_scramble == 1], na.rm = TRUE),
              tot_scramble_epa = sum(epa[qb_scramble == 1], na.rm = TRUE),
              tot_pass = sum(ifelse(play_type == "pass", 1, 0)),
              tot_pass_epa = sum(epa[play_type == "pass"], na.rm = TRUE),
              tot_rush = sum(ifelse(play_type == "run", 1, 0)),
              tot_rush_epa = sum(epa[play_type == "run"], na.rm = TRUE),
              tot_fumble = sum(sack_fumbles + rush_fumbles, na.rm = T),
              tot_fumble_lost = sum(sack_fumbles_lost + rush_fumbles_lost, na.rm = T),
              tot_turnover = sum(tot_fumble_lost, interception),
              tot_touchdown = sum(touchdown, na.rm = TRUE),
              nflfastr_attempts = sum(complete_pass, incomplete_pass, interception, na.rm = TRUE),
              ten_yard_completions = sum(ifelse(yards_gained >= 10 & complete_pass == 1, 1, 0), na.rm = TRUE),
              twenty_yard_completions = sum(ifelse(yards_gained >= 20 & complete_pass == 1, 1, 0), na.rm = TRUE),
              twenty_air_yard_attempts = sum(pass_attempt[air_yards >= 20], na.rm = TRUE),
              first_down_pass = sum(first_down_pass, na.rm = TRUE),
              att_seconds_per_play = sum(drive_possession_seconds, na.rm = TRUE) / sum(drive_play_count, na.rm = TRUE) * data_attempts,
              team_color = last(team_color)) %>%
    #filter(!is.na(attempts)) %>%
    left_join(ngs, by = c("id" = "player_gsis_id", "week")) %>%
    left_join(players, by = c('id' = 'gsis_id')) %>%
    left_join(combine, by = c("display_name" = "player_name")) %>%
    left_join(contracts, by = c("display_name" = "player")) %>%
    filter(!is.na(id)) %>%
    mutate(Quarterback = paste(display_name, " (", team_abbr, ")", sep = ""))

  
  
  
  data_test <- data %>%
    group_by(id) %>%
     reframe(attemptss = sum(attempts, na.rm = T),
             comp = sum(completions, na.rm = T),
             yards = sum(passing_yards, na.rm = T),
             td = sum(pass_touchdown, na.rm = T),
             plays = sum(data_attempts, na.rm = T),
             epa = sum(qb_epa, na.rm = T) / plays)
    
  
  
  rbsdm_test <- load_pbp() %>%
    filter(!is.na(down), !is.na(epa), pass+rush == 1) %>%
    filter(week <= 18) %>%
    group_by(id, name) %>%
    reframe(plays = n(),
            epa = mean(qb_epa)) %>%
    filter(plays >= 320)
  
#  pass_data_test <- pass_data %>%
  #  filter(team_abbr == "DET", week == 17, qtr == 4, down == 1) %>%
#    group_by(passer_player_id, Quarterback) %>%
    # filter(team_abbr == "DET", week == 17, qtr == 4, down == 1) %>%
 #   reframe(attemptss = sum(attempts))
  
#  test <- load_pbp() %>%
 #   filter(is.na(id)) %>%
  #  mutate(home = ifelse(home_team == posteam, 1, 0),
#           redzone = ifelse(yardline_100 <= 20, 1, 0),
#           garbage = ifelse(wp <= 0.1 | wp >= 0.9, 1, 0)) %>%
#    filter(posteam == "DET", week == 17, qtr == 4, down == 1) %>%
#    select(passer_player_id, id, passer_player_name, garbage, redzone, desc, complete_pass, incomplete_pass, interception)
  
  saveRDS(data, "2023_pbp_ngs_df_new.rds")
  
  
