library(tidyverse)


# load static datasets
combine_data_all <- readRDS("data-raw/combine_data_all.RDS")
conference_info <- readRDS("data-raw/conference_info.RDS")
draft_data_all <- readRDS("data-raw/draft_data_all.RDS")
combine_json_data <- readRDS("data-raw/combine_json_data.RDS")
team_info <- readRDS("data-raw/team_info.RDS")
player_info_all <- readRDS("data-raw/player_info_all.RDS")


player_info <- player_info_all %>%
  group_by(athlete_id) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  select(-rownum, -position) %>%
  rename(school = team, player=name) %>%
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
  distinct(player=full_name, school=college, player_image_nfl2=headshot_url) %>%
  group_by(player) %>%
  slice(1) %>%
  ungroup()





##### combine data cleanup
combine_clean <- combine_data_all %>%
  mutate(school = case_when(
    school == "Miami" ~ "Miami (FL)",
    school == "Miami (Ohio)" ~ "Miami (OH)",
    school == "Ala-Birmingham" ~ "Alabama-Birmingham",
    school == "Boston Col." ~ "Boston College",
    school == "NW State (LA)" ~ "Northwestern State (LA)",
    TRUE ~ as.character(school)
  )) %>%
  left_join(player_info, by = c("player", "school")) %>%
  left_join(player_info_nfl, by="athlete_id") %>%
  left_join(combine_json_data, c("player", "combine_year")) %>%
  left_join(player_info_nfl2, by=c("player", "school")) %>%
  group_by(player, school, position) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  select(-rownum) %>%
  mutate(player_image = case_when(
    !is.na(player_image_nfl) ~ player_image_nfl,
    is.na(player_image_nfl) & !is.na(player_image_nfl1) ~ player_image_nfl1,
    is.na(player_image_nfl) & is.na(player_image_nfl1) & !is.na(player_image_nfl2) ~ player_image_nfl2,
    is.na(player_image_nfl) & is.na(player_image_nfl1) & is.na(player_image_nfl2) & !is.na(player_image_cfb) ~ player_image_cfb,
    TRUE ~ as.character("http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/STA573574.png")
  )) %>%
  left_join(
    team_info %>%
      distinct(school, logo_light),
    by="school"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name1) %>%
      distinct(school=alt_name1, logo_light_1=logo_light),
    by="school"
  ) %>%
  left_join(
    team_info %>%
      drop_na(alt_name3) %>%
      distinct(school=alt_name3, logo_light_3=logo_light),
    by="school"
  ) %>%
  left_join(
    conference_info %>%
      distinct(school=ncaa_name, conference, url_conference, logo_light_4=logos),
    by="school"
  ) %>%
  ungroup() %>%
  left_join(
    conference_info %>%
      distinct(school=reference_name,conference_1=conference, url_conference_1=url_conference, logo_light_5=logos),
    by="school"
  ) %>%
  mutate(logo_light = case_when(
    !is.na(logo_light) ~ logo_light,
    is.na(logo_light) & !is.na(logo_light_1) ~ logo_light_1,
    is.na(logo_light) &  is.na(logo_light_1) & !is.na(logo_light_3) ~ logo_light_3,
    is.na(logo_light) &  is.na(logo_light_1) & is.na(logo_light_3) & !is.na(logo_light_4) ~ logo_light_4,
    is.na(logo_light) &  is.na(logo_light_1) & is.na(logo_light_3) & is.na(logo_light_4) & !is.na(logo_light_5) ~ logo_light_5,
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
  select(-c(logo_light_1, logo_light_3,logo_light_4, logo_light_5,conference_1, url_conference_1)) %>%
  ungroup() %>%
  mutate(logo_light = case_when(
    grepl("Culver", school) ~ paste0("https://d21gd0ap5v1ndt.cloudfront.net/web02/csc/images_web/headerLogo.png"),
    TRUE ~ as.character(logo_light)
  )) %>%
  mutate(team_color = ifelse(is.na(team_color), "#000000", team_color)) %>%
  mutate(position_cfb = case_when(
    position == "OG" ~ "G",
    position %in% c("OLB", "ILB") ~ "LB",
    position == "EDGE" ~ "DE",
    position == "PK" ~ "K",
    position == "NT" ~ "DL",
    TRUE ~ as.character(position)
  )) %>%
  mutate(designation = case_when(
    position2 %in% c("DL", "OL") ~ "Big",
    position2 %in% c("QB", "TE", "RB", "WR", "LB", "DB") ~ "Skill",
    position2 %in% c("LS", "PK") ~ "Special Teams",
    TRUE ~ as.character(position2)
  ))

combine_all <- combine_clean %>%
  select(
    1:23,
    team_color, team_color_secondary,
    conference,
    designation,
    player_image, team_image=logo_light, conference_image=url_conference
  ) %>%
  select(
    player, combine_year, draft_year, team=school, conference,
    draft_team,
    designation, position, position2,
    everything()
  )


#write .rda file
usethis::use_data(combine_all, overwrite = TRUE)
