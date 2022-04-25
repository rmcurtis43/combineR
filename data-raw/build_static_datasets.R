library(tidyverse)
library(tictoc)
library(cfbfastR)
library(cfbplotR)
library(combineR)
library(iVoNcaa)
library(tidyjson)
library(rjson)


########### combine data all ####################

combine_data_all <- data.frame()
for (i in 2000:2022) {
  combine_data_all <- combine_data_all %>%
    bind_rows(
      xml2::read_html(paste0("https://www.pro-football-reference.com/draft/",i,"-combine.htm")) %>%
        rvest::html_nodes("#combine") %>%
        rvest::html_table() %>%
        dplyr::first() %>%
        janitor::clean_names() %>%
        #remove, not relevant
        dplyr::select(-college) %>%
        #filter out player na
        dplyr::filter(player != "Player") %>%
        dplyr::mutate_all(na_if,"") %>%
        #separate text for numeric conversion
        tidyr::separate(ht, c("ht_ft", "ht_in"), sep = "-", fill = "right") %>%
        #convert to numeric
        dplyr::mutate_at(vars("ht_ft", "ht_in", "wt", "x40yd", "vertical", "bench", "broad_jump", "x3cone", "shuttle"), as.numeric) %>%
        #retain string
        dplyr::mutate(height_ft_in = case_when(
          !is.na(ht_ft) ~ paste0(ht_ft, "'",ht_in, '"'),
          TRUE ~ as.character(NA)
        )) %>%
        #height to cm
        dplyr::mutate(ht_total_in = (ht_ft*12) + ht_in) %>%
        #weight to kg
        dplyr::mutate(wt_kg = wt*0.45359237) %>%
        #rename weight to lbs
        dplyr::rename(wt_lbs = wt) %>%
        #vertical to in
        dplyr::rename(vertical_in = vertical) %>%
        #verical to cm
        dplyr::mutate(vertical_cm = vertical_in*2.54) %>%
        #broad jump to cm
        dplyr::mutate(broad_jump_cm = broad_jump*2.54) %>%
        #rename broad jump to broad_jump_in
        dplyr::rename(broad_jump_in = broad_jump) %>%
        #convert broad jump to peak power using Mann et al. (2021) equation (Power [W] = 32.49·Broad Jump [cm] + 39.69·Wt [kg] − 7608)
        mutate(broad_jump_power_w = (32.49*broad_jump_cm) + (39.69*wt_kg) - 7608) %>%
        #convert vertical jump to power using Sayers Equation
        mutate(vertical_jump_power_w = (60.7*(vertical_cm))+(45.3*wt_kg)-2055) %>%
        #separate draft team, draft round, and draft year
        tidyr::separate(drafted_tm_rnd_yr, c("draft_team", "draft_round", "draft_overall_pick", "draft_year"), sep = "/", fill = "warn") %>%
        #remove trailing whitespace
        dplyr::mutate(draft_team = trimws(draft_team, "r")) %>%
        #convert St. to State
        dplyr::mutate(school = str_replace(school, "St\\.", "State")) %>%
        #remove draft round suffix
        dplyr::mutate(draft_round = as.numeric(gsub("(\\d)(st|nd|rd|th)\\b", "\\1", draft_round))) %>%
        #remove draft pick suffix
        dplyr::mutate(draft_overall_pick = as.numeric(gsub("(\\d)(st pick|nd pick|rd pick|th pick)\\b", "\\1", draft_overall_pick))) %>%
        dplyr::mutate(draft_year = as.numeric(draft_year)) %>%
        dplyr::mutate(pos = case_when(

          TRUE ~ as.character(pos)
        )) %>%
        #create general list of positions ("QB" "TE" "WR" "OL" "DB" "LB" "DL" "RB" "LS" "PK")
        dplyr::mutate(pos2 = case_when(
          pos == "T" ~ "OL",
          pos == "NT" ~ "DL",
          pos == "P" | pos == "K" ~ "PK",
          pos == "EDGE" ~ "DL",
          pos == "OG" ~ "OL",
          pos == "ILB" | pos == "OLB" ~ "LB",
          pos == "DE" ~ "DL",
          pos == "DT" ~ "DL",
          pos == "OT" ~ "OL",
          pos == "C" ~ "OL",
          pos == "CB" ~ "DB",
          pos == "S" ~ "DB",
          pos == "FB" ~ "RB",
          TRUE ~ as.character(pos)
        )) %>%
        # change past team names to current active team
        dplyr::mutate(draft_team = case_when(
          draft_team == 'Washington Redskins' ~ 'Washington Football Team',
          draft_team == 'Oakland Raiders' ~ 'Las Vegas Raiders',
          draft_team == 'San Diego Chargers' ~ 'Los Angeles Chargers',
          draft_team == 'St. Louis Rams' ~ 'Los Angeles Rams',
          TRUE ~ as.character(draft_team)
        )) %>%
        dplyr::select(-ht_ft, -ht_in) %>%
        dplyr::mutate(combine_year = i) %>%
        dplyr::select(player, combine_year, draft_year, school, draft_team, draft_round, draft_overall_pick, position=pos, position2=pos2,
                      height_in=ht_total_in, height_ft_in,
                      weight_lbs = wt_lbs, weight_kg = wt_kg,
                      vertical_in, vertical_cm, vertical_jump_power_w,
                      broad_jump_in, broad_jump_cm, broad_jump_power_w,
                      bench,
                      x3cone,
                      shuttle,
                      x40yd
        )) %>%
    dplyr::arrange(draft_overall_pick)

}


saveRDS(combine_data_all, "data-raw/combine_data_all.RDS")
combine_data_all <- readRDS("data-raw/combine_data_all.RDS")


###############################################################


############# draft history data ###########
draft_info_all <- data.frame()
for (i in 1936:2021) {
  draft_info_all <- draft_info_all %>%
    bind_rows(
      rvest::read_html(paste0("https://www.drafthistory.com/index.php/years/",i)) %>%
        rvest::html_nodes("table:nth-child(2)") %>%
        rvest::html_table() %>%
        dplyr::first() %>%
        janitor::clean_names() %>%
        slice(-1) %>%
        janitor::row_to_names(1) %>%
        mutate_at(vars(1:3), as.numeric) %>%
        tidyr::fill(Round) %>%
        mutate(Year = i) %>%
        select(Year, everything())
    ) %>%
    arrange(desc(Year), Player)
}


draft_data_all <- draft_info_all %>%
    select(draft_year=Year, draft_round=Round, draft_round_pick=Pick, draft_overall_pick=Player, player=Name, draft_team=Team, position=Position, team=College) %>%
    arrange(desc(draft_year), draft_overall_pick)




saveRDS(draft_data_all, "data-raw/draft_data_all.RDS")
draft_data_all <- readRDS("data-raw/draft_data_all.RDS")



###### ~75 min to run #######################################
#### Create CFB player info dataframe ########################
combine_data_distinct <- combine_data_all %>%
  distinct(player)

tic("player_info_load")
player_info_all <- data.frame()
for (i in combine_data_distinct$player) {

  player_info_all <-  player_info_all %>%
    bind_rows(
      cfbfastR::cfbd_player_info(search_term = i)
    )
}
toc()

saveRDS(player_info_all, "data-raw/player_info_all.RDS")
player_info_all <- readRDS("data-raw/player_info_all.RDS")
##############################################################

### Create NFL player info dataframe #########################


player_info_nfl <- nflfastR::fast_scraper_roster(2000:2021)

saveRDS(player_info_nfl, "data-raw/player_info_nfl.RDS")
player_info_nfl <- readRDS("data-raw/player_info_nfl.RDS")
###############################################################

### Team Info #########################

team_info <- cfbfastR::cfbd_team_info() %>%
  select(school, alt_name1, alt_name2, alt_name3, logos) %>%
  mutate(logo_light = map(logos, pluck, 1) %>% reduce(c)) %>%
  mutate(logo_dark = map(logos, pluck, last) %>% reduce(c)) %>%
  mutate(alt_name1 = case_when(
    school == 'Ole Miss' ~ 'Mississippi',
    school == 'Boston College' ~ 'Boston Col.',
    TRUE ~ as.character(alt_name1)
  )) %>%
  mutate(alt_name1 = ifelse(alt_name1 == school, NA, alt_name1)) %>%
  mutate(alt_name2 = ifelse(alt_name2 == school | alt_name2 == alt_name1, NA, alt_name2)) %>%
  mutate(alt_name3 = ifelse(alt_name3 == school | alt_name3 == alt_name2 | alt_name3 == alt_name1, NA, alt_name3))


saveRDS(team_info, "data-raw/team_info.RDS")
team_info <- readRDS("data-raw/team_info.RDS")





###############################################################

### Conference Info #########################

conference_info <- iVoNcaa::get_ncaa()

conference_info <- conference_info %>%
  mutate(ncaa_name = str_trim(ncaa_name)) %>%
  mutate(reference_name = str_trim(reference_name)) %>%
  mutate(ncaa_name = case_when(
    name == "Southern California Trojans" ~ paste0("USC"),
    ncaa_name == "Boston College" ~ "Boston Col.",
    ncaa_name == "Southern Miss." ~ "Southern Miss",
    ncaa_name == "Middle Tenn." ~ "Middle Tennessee State",
    ncaa_name == "Louisiana-Monroe" ~ "La-Monroe",
    TRUE ~ as.character(ncaa_name)
  ))

saveRDS(conference_info, "data-raw/conference_info.RDS")
conference_info <- readRDS("data-raw/conference_info.RDS")
###############################################################

### Draft Info #########################


draft_info_all <- data.frame()
for (i in 1936:2021) {
  draft_info_all <- draft_info_all %>%
    bind_rows(
      rvest::read_html(paste0("https://www.drafthistory.com/index.php/years/",i)) %>%
        rvest::html_nodes("table:nth-child(2)") %>%
        rvest::html_table() %>%
        dplyr::first() %>%
        janitor::clean_names() %>%
        slice(-1) %>%
        janitor::row_to_names(1) %>%
        mutate_at(vars(1:3), as.numeric) %>%
        tidyr::fill(Round) %>%
        mutate(Year = i) %>%
        select(Year, everything())
    ) %>%
    arrange(desc(Year), Player)
}

saveRDS(draft_info_all, "data-raw/draft_info_all.RDS")
draft_info_all <- readRDS("data-raw/draft_info_all.RDS")

######### nfl.com data###############

result <- fromJSON(file = "data/combine_2022.json")

json_data_frame <- as.data.frame(result)

combine_json_data <- result$combineProfiles %>%
  spread_all %>%
  as_tibble() %>%
  mutate(combine_year = 2022) %>%
  mutate(headshot = paste0('https://static.www.nfl.com/image/private/f_png,q_85,h_118,w_118,c_fill,g_face:center,f_auto/league/god-combine-headshots/2022/',id )) %>%
  select(player=person.displayName, combine_year, player_image_nfl1=headshot)

saveRDS(combine_json_data, "data-raw/combine_json_data.RDS")
combine_json_data <- readRDS("data-raw/combine_json_data.RDS")





