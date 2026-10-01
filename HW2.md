HW2
================
Yifan Wan
2026-10-01

# Problem 1

``` r
library(tidyverse)

transit_df <- read_csv(
  "Data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
  na = c("", "NA"),
  show_col_types = FALSE
) |> 
  select(
    line = Line,
    station = `Station Name`,
    latitude = `Station Latitude`,
    longitude = `Station Longitude`,
    starts_with("Route"),
    entry = Entry,
    vending = Vending,
    entrance_type = `Entrance Type`,
    ada = ADA
  ) |> 
  mutate(
    entry = case_match(
      entry,
      "YES" ~ TRUE,
      "NO" ~ FALSE,
      .default = NA
    ),
    ada = as.logical(ada)
  )
```

``` r
dim(transit_df)
```

    ## [1] 1868   19

``` r
stations <- transit_df |> 
  group_by(line, station) |> 
  summarise(
    ada = any(ada, na.rm = TRUE),
    .groups = "drop"
  )

nrow(stations)
```

    ## [1] 465

``` r
stations |> 
  summarise(n_ada_stations = sum(ada))
```

    ## # A tibble: 1 × 1
    ##   n_ada_stations
    ##            <int>
    ## 1             84

``` r
transit_df |> 
  filter(vending == "NO") |> 
  summarise(proportion_allow_entry = mean(entry, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   proportion_allow_entry
    ##                    <dbl>
    ## 1                  0.377

``` r
transit_routes <- transit_df |>
  mutate(
    across(starts_with("Route"), as.character)
  ) |>
  pivot_longer(
    cols = starts_with("Route"),
    names_to = "route_number",
    values_to = "route_name",
    values_drop_na = TRUE
  ) |>
  mutate(
    route_number = readr::parse_number(route_number)
  )

a_stations <- transit_routes |> 
  filter(route_name == "A") |> 
  distinct(line, station) |> 
  inner_join(stations, by = c("line", "station"))

a_stations |> 
  summarise(
    n_a_stations = n(),
    n_a_ada_stations = sum(ada)
  )
```

    ## # A tibble: 1 × 2
    ##   n_a_stations n_a_ada_stations
    ##          <int>            <int>
    ## 1           60               17
