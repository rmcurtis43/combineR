library(tidyverse)
library(zoo)


# data load

conference_info <- readRDS("data-raw/conference_info.RDS")
draft_data_all <- readRDS("data-raw/draft_data_all.RDS")
team_info <- readRDS("data-raw/team_info.RDS")
combine_json_data <- readRDS("data-raw/combine_json_data.RDS")
team_info <- readRDS("data-raw/team_info.RDS")
player_info_all <- readRDS("data-raw/player_info_all.RDS")
###############################

# player data for images
player_info <- player_info_all %>%
  group_by(athlete_id) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  select(-rownum, -position) %>%
  rename(player=name) %>%
  mutate(player_image_cfb = ifelse(grepl('-', athlete_id),
                                   paste0("http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/STA573574.png"),
                                   paste0("http://a.espncdn.com/i/headshots/college-football/players/full/",athlete_id,".png")
  ))

suppressWarnings(
  player_info_nfl <- readRDS("data-raw/player_info_nfl.RDS") %>%
    drop_na(espn_id) %>%
    mutate(athlete_id = as.character(espn_id)) %>%
    select(athlete_id, player_image_nfl=headshot_url, years_exp) %>%
    group_by(athlete_id) %>%
    summarise(
      player_image_nfl = first(player_image_nfl),
      nfl_years_exp = max(years_exp, na.rm = TRUE)) %>%

    ungroup() %>%
    mutate_if(is.numeric, list(~na_if(., -Inf)))
)

player_info_nfl2 <- readRDS("data-raw/player_info_nfl.RDS") %>%
  distinct(player=full_name, team=college, player_image_nfl2=headshot_url) %>%
  group_by(player) %>%
  slice(1) %>%
  ungroup()



# data cleaning

draft_data_clean <- draft_data_all %>%
  left_join(player_info, by = c("player", "team")) %>%
  left_join(player_info_nfl, by="athlete_id") %>%
  left_join(combine_json_data %>% rename(draft_year=combine_year), c("player", "draft_year")) %>%
  left_join(player_info_nfl2, by=c("player", "team")) %>%
  group_by(player, team, position) %>%
  mutate(player_image = case_when(
    !is.na(player_image_nfl) ~ player_image_nfl,
    is.na(player_image_nfl) & !is.na(player_image_nfl1) ~ player_image_nfl1,
    is.na(player_image_nfl) & is.na(player_image_nfl1) & !is.na(player_image_nfl2) ~ player_image_nfl2,
    is.na(player_image_nfl) & is.na(player_image_nfl1) & is.na(player_image_nfl2) & !is.na(player_image_cfb) ~ player_image_cfb,
    TRUE ~ as.character("http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/STA573574.png")
  )) %>%
  left_join(
    team_info %>%
      distinct(team=school, team_logo=logo_light),
    by="team"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name1) %>%
      distinct(team=alt_name1, team_logo_1=logo_light),
    by="team"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name3) %>%
      distinct(team=alt_name3, team_logo_3=logo_light),
    by="team"
  ) %>%
  left_join(
    conference_info %>%
      distinct(team=ncaa_name, conference, url_conference, team_logo_4=logos),
    by="team"
  ) %>%
  ungroup() %>%
  left_join(
    conference_info %>%
      distinct(team=reference_name,conference_1=conference, url_conference_1=url_conference, team_logo_5=logos),
    by="team"
  ) %>%
  mutate(team_logo = case_when(
    !is.na(team_logo) ~ team_logo,
    is.na(team_logo) & !is.na(team_logo_1) ~ team_logo_1,
    is.na(team_logo) &  is.na(team_logo_1) & !is.na(team_logo_3) ~ team_logo_3,
    is.na(team_logo) &  is.na(team_logo_1) & is.na(team_logo_3) & !is.na(team_logo_4) ~ team_logo_4,
    is.na(team_logo) &  is.na(team_logo_1) & is.na(team_logo_3) & is.na(team_logo_4) & !is.na(team_logo_5) ~ team_logo_5,
    TRUE ~ as.character("https://www.freepnglogos.com/uploads/ncaa-png-logo/ncaa-png-logo-0.png")
  )) %>%
  mutate(conference = case_when(
    !is.na(conference) ~ conference,
    is.na(conference) & !is.na(conference_1) ~ conference_1,
    TRUE ~ as.character(NA_character_)
  )) %>%
  mutate(url_conference = case_when(
    !is.na(url_conference) ~ url_conference,
    is.na(url_conference) & !is.na(url_conference_1) ~ url_conference_1,
    TRUE ~ as.character(NA_character_)
  )) %>%
  select(-c(team_logo_1, team_logo_3,team_logo_4, team_logo_5,conference_1, url_conference_1)) %>%
  mutate(team_logo = case_when(
    grepl("Culver", team) ~ paste0("https://d21gd0ap5v1ndt.cloudfront.net/web02/csc/images_web/headerLogo.png"),
    grepl("Louisiana-Lafayette	", team) ~ paste0("https://rebelnation1.s3.amazonaws.com/wp-content/uploads/2017/12/wsi-imageoptim-louisiana-lafayette-ragin-cajuns-logo.png"),
    TRUE ~ as.character(team_logo)
  )) %>%
  select(draft_year, draft_round, draft_round_pick, draft_overall_pick, player, draft_team, position, team, player_image, team_image=team_logo, conference_image=url_conference)


draft_all <- draft_data_clean



#write .rda file
usethis::use_data(draft_all, overwrite = TRUE)

# write RDS to data-raw for draft_data() function
saveRDS(draft_all, "data-raw/draft_all.RDS")

