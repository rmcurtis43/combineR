
#' Pull NFL Combine Data from Pro Football Reference
#' @param start_year Year. Start year of data gather (e.g. 2020). Data is only available starting from the year 2000.  If no start_year or end_year is specific, data from 2000-current is pulled.
#' @param end_year Year. End year of data gather (e.g. 2021).
#' @return A data frame with descriptive statistics. If you are only interested in certain columns
#' you can add these columns.
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import rvest
#' @import janitor
#' @import xml2
#' @import stringr
#' @export
#' @examples
#' \dontrun{
#' pull_combine_data(
#' start_year = 2019,
#' end_year = 2021
#' )
#' }


pull_combine_data <- function(start_year = 2000, end_year = 2022) {
  combine_year_data <-  function(year) {

    data <- xml2::read_html(paste0("https://www.pro-football-reference.com/draft/",year,"-combine.htm")) %>%
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
      dplyr::select(player, draft_year, school, draft_team, draft_round, draft_overall_pick, position=pos, position2=pos2,
             height_in=ht_total_in, height_ft_in,
             weight_lbs = wt_lbs, weight_kg = wt_kg,
             vertical_in, vertical_cm,
             broad_jump_in, broad_jump_cm,
             bench,
             x3cone,
             shuttle,
             x40yd
      ) %>%
      dplyr::arrange(draft_overall_pick)

    return(data)
  }

  data <- c(start_year:end_year) %>%
    purrr::map_dfr(
      ~combine_year_data(.x)
    )
  return(data)
}
