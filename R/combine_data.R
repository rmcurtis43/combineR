
#' Extract NFL Combine Data w/ Images
#' @param start_year Year. Start year of data gather (e.g. 2020). Data is only available starting from the year 2000.  If no start_year or end_year is specific, data from 2000-current is pulled.
#' @param end_year Year. End year of data gather (e.g. 2022).
#' @return A data frame with descriptive statistics. If you are only interested in certain columns
#' you can add these columns.
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' combine_data(
#' start_year = 2019,
#' end_year = 2021
#' )
#' }


combine_data <- function(start_year = 2000, end_year = 2022) {

  #load("data/combine_all.rda")

  data <- readRDS(url("https://github.com/rmcurtis43/combineR/blob/main/data-raw/combine_all.RDS?raw=true")) %>%
    dplyr::filter(combine_year %in% c(start_year:end_year)) %>%
    dplyr::arrange(desc(combine_year), draft_overall_pick)

return(data)

}


