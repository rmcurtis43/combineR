
# combine_data variables
utils::globalVariables(c(
  "player", "combine_year", "draft_year", "team", "school", "conference",
  "draft_team", "designation", "position", "position2", "draft_round",
  "draft_overall_pick", "height_in", "height_ft_in", "weight_lbs",
  "weight_kg", "vertical_in", "vertical_cm", "vertical_jump_power_w",
  "broad_jump_in", "broad_jump_cm", "broad_jump_power_w", "bench",
  "x3cone", "shuttle", "x40yd", "team_color", "team_color_secondary",
  "player_image", "team_image", "conference_image"
))



# draft_data variables
utils::globalVariables(c(
  "draft_year", "draft_round", "draft_round_pick", "draft_overall_pick",
  "player", "draft_team", "position", "team", "player_image", "team_image",
  "conference_image"
))


# benchmark_table variables
utils::globalVariables(c(
  "Test", "Value", "sd", "pnorm",
  "Height", "Bench", "3cone", "40yd", "Percentile", "rownum",
  "value", "value_m1", "value_p1", "value_final", "Class", "rownum"
))


# pull_combine_data variables
utils::globalVariables(c(
  "player", "combine_year", "draft_year", "school", "draft_team",
  "draft_round", "draft_overall_pick", "position", "position2",
  "height_in", "height_ft_in", "weight_lbs", "weight_kg", "vertical_in",
  "vertical_cm", "broad_jump_in", "broad_jump_cm", "bench", "x3cone",
  "shuttle", "x40yd", "college", "ht", "ht_ft", "ht_in", "wt", "vertical",
  "broad_jump", "wt_kg", "drafted_tm_rnd_yr", "pos", "pos2", "ht_total_in", "wt_lbs"
))



#  variables
utils::globalVariables(c(
  "combine_all", "draft_all"
))
