#' Produce a table that returns a single test's value for each user selection position by percentile bin.
#' @param test character string. Start year of data gather (e.g. 2020). Data is only available starting from the year 2000.  If no start_year or end_year is specific, data from 2000-current is pulled.
#' @param positions character string. User defined positions to display percentiles for (e.g., 'DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS').
#' @return A formatted table with combine values by percentile per position.
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import gt
#' @export
#' @examples
#' \dontrun{
#' benchmark_table (
#' test = '40yd',
#' positions = c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR')
#' )
#' }


benchmark_table <- function(test = '40yd', positions = c('DB', 'DL', 'LB', 'OL', 'QB', 'RB', 'TE', 'WR', 'PK', 'LS')){

  data <- pull_combine_data()

  percentile_data <- data %>%
    select(position=position2, height=height_in, weight=weight_lbs, vertical=vertical_in, broad_jump=broad_jump_in, bench, `3cone`=x3cone, shuttle, `40yd`=x40yd) %>%
    pivot_longer(!position, names_to = "Test", values_to = "Value", values_drop_na = TRUE) %>%
    left_join(data %>%
                select(position=position2, height=height_in, weight=weight_lbs, vertical=vertical_in, broad_jump=broad_jump_in, bench, `3cone`=x3cone, shuttle, `40yd`=x40yd) %>%
                pivot_longer(!position, names_to = "Test", values_to = "Value", values_drop_na = TRUE) %>%
                group_by(position, Test) %>%
                summarise(
                  mean = mean(Value),
                  sd = sd(Value),
                  count = n()
                ), c('position', 'Test')) %>%
    mutate(percentile = round((
      pnorm(Value, mean = mean, sd = sd) *
        100
    ), 0))


  p <- c(0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 1)
  p_names <- map_chr(p, ~paste0(.x*100, "th"))
  p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)


  p_funs_rev <- map(p, ~partial(quantile, probs = (1-.x), na.rm = TRUE)) %>%
    set_names(nm = p_names)


  percentile_data_fwd <- data %>%
    select(position=position2, Height=height_in, Weight=weight_lbs, `Vertical Jump`=vertical_in, `Broad Jump`=broad_jump_in, Bench=bench) %>%
    group_by(position) %>%
    summarize_at(vars(Height:Bench), funs(!!!p_funs))


  percentile_data_rev <- data %>%
    select(position=position2,`3cone`=x3cone, Shuttle=shuttle, `40yd`=x40yd) %>%
    group_by(position) %>%
    summarize_at(vars(`3cone`:`40yd`), funs(!!!p_funs_rev))

  percentile_data <- percentile_data_fwd %>%
    cbind(percentile_data_rev) %>%
    pivot_longer(!position, names_to = "test", values_to = "value") %>%
    separate(test, c('Test', 'Percentile'), sep = '_') %>%
    arrange(position, Test, Percentile) %>%
    mutate(rownum = case_when(
      Percentile == '5th' ~ 1,
      Percentile == '10th' ~ 2,
      Percentile == '20th' ~ 3,
      Percentile == '40th' ~ 4,
      Percentile == '60th' ~ 5,
      Percentile == '80th' ~ 6,
      Percentile == '90th' ~ 7,
      Percentile == '100th' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    group_by(position, Test) %>%
    arrange(position, Test, desc(rownum)) %>%
    select(-rownum) %>%
    ungroup()

  ord <- c('5th', '10th', '20th', '40th', '60th', '80th', '90th', '100th')

  metric <- ifelse(test == 'Height', '[in]',
                   ifelse(test == 'Weight', '[lbs]',
                          ifelse(test %in% c('Vertical Jump', 'Broad Jump'), '[in]',
                                 ifelse(test == 'Bench', '[reps @ 225lbs]',
                                        '[s]'))))

  ex <- percentile_data %>%
    filter(position %in% positions) %>%
    filter(Test == test) %>%
    mutate(value = ifelse(Test %in% c('Height', 'Weight', 'Vertical Jump', 'Broad Jump', 'Bench'), round(value,0), round(value, 2))) %>%
    pivot_wider(names_from = c(position), values_from = value) %>%
    mutate(Class = case_when(
      Percentile == '5th' ~ 'Bad',
      Percentile == '10th' ~ 'Very Poor',
      Percentile == '20th' ~ 'Poor',
      Percentile == '40th' ~ 'Fair',
      Percentile == '60th' ~ 'Good',
      Percentile == '80th' ~ 'Very Good',
      Percentile == '90th' ~ 'Excellent',
      Percentile == '100th' ~ 'World-Leading',
      TRUE ~ as.character(NA)
    )) %>%
    mutate(rownum = case_when(
      Percentile == '5th' ~ 1,
      Percentile == '10th' ~ 2,
      Percentile == '20th' ~ 3,
      Percentile == '40th' ~ 4,
      Percentile == '60th' ~ 5,
      Percentile == '80th' ~ 6,
      Percentile == '90th' ~ 7,
      Percentile == '100th' ~ 8,
      TRUE ~ as.numeric(NA)
    )) %>%
    arrange(desc(rownum)) %>%
    select(-rownum) %>%
    ungroup()

  table_perc <- ex %>%
    select(-Test) %>%
    select(Percentile, Class, dplyr::everything()) %>%
    gt() %>%
    tab_header(
      title = test,
      subtitle = 'Percentile by Position'
    ) %>%
    cols_align(
      align = "center",
      columns = vars(Percentile, Class)
    ) %>%
    tab_source_note("Data Source: {combineR}/Pro Football Reference") %>%
    tab_header(
      title = md(paste0('**', test,' ',metric[1], '**')),
      subtitle = "Percentile by Position"
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FAE100"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 1)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "darkgreen"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 2)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#00CC00"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 3)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#CFFFA9"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 4)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#ffff99"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 5)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FFCE8D"),
        cell_text(color = "black")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 6)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FF8D8D"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 7)
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#D53B3B"),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = vars(Percentile, Class, !!!syms(positions)),
        rows = 8)
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "right",
          color = "lightgrey",
          weight = px(3)
        )
      ),
      locations = list(
        cells_body(
          columns = vars(Class)
        )
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "lightgrey",
          weight = px(3)
        )
      ),
      locations = list(
        cells_column_labels(
          columns = gt::everything()
        )
      )
    ) %>%
  cols_align(
    align = 'center',
    columns = gt::everything()
  )

  print(table_perc)




}

