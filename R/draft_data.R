
#' Extract NFL Draft Data w/ Images
#' @param start_year Year. Start year of data gather (e.g. 2020). Data is  available starting from the year 1936.  If no start_year or end_year is specific, data from 1936-current is pulled.
#' @param end_year Year. End year of data gather (e.g. 2022).
#' @return A data frame with descriptive statistics. If you are only interested in certain columns
#' you can add these columns.
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' draft_data(
#' start_year = 1936,
#' end_year = 2021
#' )
#' }


draft_data <- function(start_year = 1936, end_year = 2021) {

  #load("data/draft_all.rda")

  data <- readRDS(url("https://github.com/rmcurtis43/combineR/blob/main/data-raw/draft_all.RDS?raw=true")) %>%
    dplyr::filter(draft_year %in% c(start_year:end_year)) %>%
    dplyr::arrange(desc(draft_year), draft_overall_pick)

  return(data)

}


