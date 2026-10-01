HW2
================
Yifan Wan
2026-10-01

``` r
library(tidyverse)
library(readxl)
```

# Problem 1

``` r
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

The cleaned dataset contains 1,868 rows and 19 columns. Each row
represents a subway station entrance or exit. The variables include the
subway line and station name, station latitude and longitude, the routes
served, whether entry is allowed, vending availability, entrance type,
and ADA compliance. I selected the variables needed for the analysis,
treated blank values as missing, and converted entry from “YES”/“NO” to
a logical variable. I identified stations using both line and station
name, yielding 465 distinct stations. Of these, 84 are ADA compliant.
Among entrances without vending machines, 37.7% allow entry. The
original cleaned dataset is not fully tidy because routes are stored
across multiple columns (Route1–Route11). I pivoted those columns into
route number and route name variables; using that longer dataset, I
found that 60 distinct stations serve the A train, of which 17 are ADA
compliant.

# Problem 2

``` r
trash_wheel_file <- "Data/202509 Trash Wheel Collection Data.xlsx"

mr_trash_wheel <- read_excel(
  trash_wheel_file,
  sheet = "Mr. Trash Wheel",
  range = "A2:N709"
) |>
  rename(
    dumpster = Dumpster,
    month = Month,
    year = Year,
    date = Date,
    weight_tons = `Weight (tons)`,
    volume_cubic_yards = `Volume (cubic yards)`,
    plastic_bottles = `Plastic Bottles`,
    polystyrene = Polystyrene,
    cigarette_butts = `Cigarette Butts`,
    glass_bottles = `Glass Bottles`,
    plastic_bags = `Plastic Bags`,
    wrappers = Wrappers,
    sports_balls = `Sports Balls`,
    homes_powered = `Homes Powered*`
  ) |>
  mutate(
    sports_balls = as.integer(round(sports_balls)),
    trash_wheel = "Mr. Trash Wheel"
  )

professor_trash_wheel <- read_excel(
  trash_wheel_file,
  sheet = "Professor Trash Wheel",
  range = "A2:M134"
) |>
  rename(
    dumpster = Dumpster,
    month = Month,
    year = Year,
    date = Date,
    weight_tons = `Weight (tons)`,
    volume_cubic_yards = `Volume (cubic yards)`,
    plastic_bottles = `Plastic Bottles`,
    polystyrene = Polystyrene,
    cigarette_butts = `Cigarette Butts`,
    glass_bottles = `Glass Bottles`,
    plastic_bags = `Plastic Bags`,
    wrappers = Wrappers,
    homes_powered = `Homes Powered*`
  ) |>
  mutate(trash_wheel = "Professor Trash Wheel")

gwynnda_trash_wheel <- read_excel(
  trash_wheel_file,
  sheet = "Gwynns Falls Trash Wheel",
  range = "A2:L351"
) |>
  rename(
    dumpster = Dumpster,
    month = Month,
    year = Year,
    date = Date,
    weight_tons = `Weight (tons)`,
    volume_cubic_yards = `Volume (cubic yards)`,
    plastic_bottles = `Plastic Bottles`,
    polystyrene = Polystyrene,
    cigarette_butts = `Cigarette Butts`,
    plastic_bags = `Plastic Bags`,
    wrappers = Wrappers,
    homes_powered = `Homes Powered*`
  ) |>
  mutate(trash_wheel = "Gwynnda")

mr_trash_wheel <- mr_trash_wheel |>
  mutate(year = as.integer(readr::parse_number(as.character(year))))

professor_trash_wheel <- professor_trash_wheel |>
  mutate(year = as.integer(readr::parse_number(as.character(year))))

gwynnda_trash_wheel <- gwynnda_trash_wheel |>
  mutate(year = as.integer(readr::parse_number(as.character(year))))

trash_wheel_df <- bind_rows(
  mr_trash_wheel,
  professor_trash_wheel,
  gwynnda_trash_wheel
) |>
  relocate(trash_wheel, .before = dumpster)

nrow(trash_wheel_df)
```

    ## [1] 1188

``` r
professor_trash_wheel |>
  summarise(total_weight_tons = sum(weight_tons, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   total_weight_tons
    ##               <dbl>
    ## 1              282.

``` r
gwynnda_trash_wheel |>
  filter(month == "June", year == 2022) |>
  summarise(total_cigarette_butts = sum(cigarette_butts, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   total_cigarette_butts
    ##                   <dbl>
    ## 1                 18120

The combined dataset contains 1,188 observations. Each observation
represents a dumpster collection and includes variables such as the
Trash Wheel, dumpster number, collection date, weight, volume, and the
amounts of different types of trash collected. I imported the Mr. Trash
Wheel, Professor Trash Wheel, and Gwynnda worksheets, excluded summary
rows and note columns by specifying ranges in `read_excel()`, and used
consistent variable names before combining the data. I rounded Mr. Trash
Wheel’s sports ball counts to the nearest integer and converted them to
integers. Professor Trash Wheel collected a total of 282.26 tons of
trash. Gwynnda collected 18,120 cigarette butts in June 2022.
