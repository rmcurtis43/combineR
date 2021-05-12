
 <!-- badges: start -->
  [![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  <!-- badges: end -->


# {combineR}
An R package to gather NFL Draft Combine data.

<img src="man/images/combineRsticker.png" align="right" width="300" />



***

## What is combineR

**{combineR}** is a package developed to easily gather over 20 years of NFL Draft Combine data from [Pro Football Reference.](https://www.pro-football-reference.com/)

***

## Installation

``` r
#Not yet on CRAN

  
#Install the development version from GitHub  
install.packages("devtools")
devtools::install_github("rmcurtis43/combineR")
```

***

## Example

You can pull single season data by simply entering a year (e.g., `2021`) in the `pull_combine_data()` function. 


``` r
library(combineR)

pull_combine_data(2021)

`
# A tibble: 464 x 20
   player draft_year school draft_team draft_round draft_overall_p~ position
   <chr>       <dbl> <chr>  <chr>            <dbl>            <dbl> <chr>   
 1 Trevo~       2021 Clems~ Jacksonvi~           1                1 QB      
 2 Zach ~       2021 BYU    New York ~           1                2 QB      
 3 Trey ~       2021 North~ San Franc~           1                3 QB      
 4 Kyle ~       2021 Flori~ Atlanta F~           1                4 TE      
 5 Ja'Ma~       2021 LSU    Cincinnat~           1                5 WR      
 6 Penei~       2021 Oregon Detroit L~           1                7 OL      
 7 Jayce~       2021 South~ Carolina ~           1                8 DB      
 8 Patri~       2021 Alaba~ Denver Br~           1                9 DB      
 9 Justi~       2021 Ohio ~ Chicago B~           1               11 QB      
10 Micah~       2021 Penn ~ Dallas Co~           1               12 LB      
# ... with 454 more rows, and 13 more variables: position2 <chr>,
#   height_in <dbl>, height_ft_in <chr>, weight_lbs <dbl>, weight_kg <dbl>,
#   vertical_in <dbl>, vertical_cm <dbl>, broad_jump_in <dbl>,
#   broad_jump_cm <dbl>, bench <dbl>, x3cone <dbl>, shuttle <dbl>, x40yd <dbl>
`
```



Or you can enter a start_year (e.g., `2019`) and and end_year (e.g. `2021`) to gather data between user defined seasons. Please note the earliest data available is `2000`.

``` r
library(combineR)

pull_combine_data(
start_year = 2019,
end_year = 2021
)

OR

pull_combine_data(2019, 2021)

`
# A tibble: 1,137 x 20
   player draft_year school draft_team draft_round draft_overall_p~ position
   <chr>       <dbl> <chr>  <chr>            <dbl>            <dbl> <chr>   
 1 Kyler~       2019 Oklah~ Arizona C~           1                1 QB      
 2 Nick ~       2019 Ohio ~ San Franc~           1                2 DL      
 3 Quinn~       2019 Alaba~ New York ~           1                3 DL      
 4 Cleli~       2019 Clems~ Las Vegas~           1                4 EDGE    
 5 Devin~       2019 LSU    Tampa Bay~           1                5 LB      
 6 Danie~       2019 Duke   New York ~           1                6 QB      
 7 Josh ~       2019 Kentu~ Jacksonvi~           1                7 EDGE    
 8 T.J. ~       2019 Iowa   Detroit L~           1                8 TE      
 9 Ed Ol~       2019 Houst~ Buffalo B~           1                9 DL      
10 Devin~       2019 Michi~ Pittsburg~           1               10 LB      
# ... with 1,127 more rows, and 13 more variables: position2 <chr>,
#   height_in <dbl>, height_ft_in <chr>, weight_lbs <dbl>, weight_kg <dbl>,
#   vertical_in <dbl>, vertical_cm <dbl>, broad_jump_in <dbl>,
#   broad_jump_cm <dbl>, bench <dbl>, x3cone <dbl>, shuttle <dbl>, x40yd <dbl>
`
```




**Note:** Calling just `pull_combine_data()` returns a tibble of all available data (i.e., 2000-current).


***

## Data Dictionary


The current tibble returns 20 fields:

* player (Player Name, character)
* draft_year (Combine Draft Year, numeric)
* school (School Attended, character)
* draft_team (Draft Team, character)
* draft_round (Round Drafted, numeric)
* draft_overall_pick (Overall Pick in Draft Year, numeric)
* position (Position supplid by Pro Football Reference, character)
  *"QB", "DL", "CB", "OL", "LB", "WR", "RB", "S", "TE", "LS", "K", "P", "DB", "DE", "OLB", "OT", "C", "DT", "NT","PK", "FB", "T"
* position2 (Condensed Position Grouping, character)
  *"QB", "DL", "DB", "OL", "LB", "WR", "RB", "TE", "LS", "PK"
* height_in (Height in inches, numeric)
* height_ft_in (Height in feet and inches, character)
* weight_lbs (Weight in pounds, numeric)
* weight_kg (Weight in kilograms, numeric)
* vertical_in (Vertical Jump Height in inches, numeric)
* vertical_cm (Verical Jump Height in centimeters, numeric)
* broad_jump_in (Broad Jump [Standing Long Jump] in inches, numeric)
* broad_jump_cm (Broad Jump [Standing Long Jump] in centimeters, numeric)
* bench (Bench Press Reps @ 225 lbs, numeric)
* x3cone (3 Cone Agility Drill in seconds, numeric)
* shuttle (Shuttle Drill in seconds, numeric)
* x40yd (40 yard dash in seconds, numeric)


***

## Citation

```{r}

citation("combineR")

Curtis, RM. (2021). combineR: An R package to gather NFL Draft Combine data. R package version 0.4.0. https://github.com/rmcurtis43/combineR

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {combineR: An R package to gather NFL Draft Combine data},
    author = {{Ryan Curtis} and email = "rmcurtis43 at yahoo.com")},
    note = {R package version 0.4.0},
    url = {https://github.com/rmcurtis43/combineR},
  }
```

**{combineR}** is under development and may change over time.   





***

## Visualizations


You can check out some **{combineR}** visualizations [here](https://github.com/rmcurtis43/combineR-visuals)
