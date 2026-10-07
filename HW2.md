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
trash_wheel_file <- "Data/202610 Trash Wheel Collection Data.xlsx"

numeric_vars <- c(
  "dumpster", "year", "weight_tons", "volume_cubic_yards",
  "plastic_bottles", "polystyrene", "cigarette_butts",
  "glass_bottles", "plastic_bags", "wrappers",
  "sports_balls", "homes_powered"
)

clean_trash_wheel <- function(data, wheel_name) {
  data |>
    mutate(
      across(
        any_of(numeric_vars),
        ~ readr::parse_number(as.character(.x))
      ),
      dumpster = as.integer(dumpster),
      year = as.integer(year),
      trash_wheel = wheel_name
    )
}

mr_trash_wheel <- read_excel(
  trash_wheel_file,
  sheet = "Mr. Trash Wheel",
  range = "A2:N742"
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
  clean_trash_wheel("Mr. Trash Wheel") |> 
  mutate(
    sports_balls = as.integer(round(sports_balls))
  )

professor_trash_wheel <- read_excel(
  trash_wheel_file,
  sheet = "Professor Trash Wheel",
  range = "A2:M139",
  na = c("", "dive")
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
  clean_trash_wheel("Professor Trash Wheel")

gwynnda_trash_wheel <- read_excel(
  trash_wheel_file,
  sheet = "Gwynnda the Good Wheel of the W",
  range = "A2:L394"
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
  clean_trash_wheel("Gwynnda")

trash_wheel_df <- bind_rows(
  mr_trash_wheel,
  professor_trash_wheel,
  gwynnda_trash_wheel
) |>
  relocate(trash_wheel, .before = dumpster)

nrow(trash_wheel_df)
```

    ## [1] 1269

``` r
professor_trash_wheel |>
  summarise(total_weight_tons = sum(weight_tons, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   total_weight_tons
    ##               <dbl>
    ## 1              295.

``` r
gwynnda_trash_wheel |>
  filter(month == "June", year == 2022) |>
  summarise(total_cigarette_butts = sum(cigarette_butts, na.rm = TRUE))
```

    ## # A tibble: 1 × 1
    ##   total_cigarette_butts
    ##                   <dbl>
    ## 1                 18120

The combined dataset contains 1,269 observations. Each observation
represents a dumpster collection and includes variables such as the
Trash Wheel, dumpster number, collection date, weight, volume, and the
amounts of different types of trash collected. I imported the Mr. Trash
Wheel, Professor Trash Wheel, and Gwynnda worksheets, excluded summary
rows and note columns by specifying ranges in `read_excel()`, and used
consistent variable names before combining the data. I rounded Mr. Trash
Wheel’s sports ball counts to the nearest integer and converted them to
integers. One text extry in the Professor Trash Wheel’s plastic bottle
count was treated as missing. Professor Trash Wheel collected a total of
295.96 tons of trash. Gwynnda collected 18,120 cigarette butts in June
2022.

# Problem 3

``` r
mci_baseline <- read_csv(
  "Data/data_mci/MCI_baseline.csv",
  skip = 1,
  na = c("", ".", "NA"),
  show_col_types = FALSE
) |>
  rename(
    id = ID,
    baseline_age = `Current Age`,
    sex = Sex,
    education = Education,
    mci_onset_age = `Age at onset`
  ) |>
  filter(
    is.na(mci_onset_age) | mci_onset_age > baseline_age
  ) |>
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    apoe4 = factor(apoe4, levels = c(0, 1),
                   labels = c("Non-carrier", "Carrier")),
    developed_mci = !is.na(mci_onset_age)
  )
```

``` r
mci_baseline |>
  summarise(
    n_participants = n(),
    n_developed_mci = sum(developed_mci),
    average_baseline_age = mean(baseline_age),
    proportion_women_apoe4_carriers =
      mean(apoe4[sex == "Female"] == "Carrier")
  )
```

    ## # A tibble: 1 × 4
    ##   n_participants n_developed_mci average_baseline_age proportion_women_apoe4_c…¹
    ##            <int>           <int>                <dbl>                      <dbl>
    ## 1            479              93                 65.0                        0.3
    ## # ℹ abbreviated name: ¹​proportion_women_apoe4_carriers

``` r
mci_amyloid <- read_csv(
  "Data/data_mci/mci_amyloid.csv",
  skip = 1,
  na = c("", "NA"),
  show_col_types = FALSE
) |>
  rename(id = `Study ID`) |>
  pivot_longer(
    cols = -id,
    names_to = "time",
    values_to = "amyloid_42_40"
  ) |>
  mutate(
    time = case_when(
      time == "Baseline" ~ 0,
      TRUE ~ readr::parse_number(time)
    )
  )
```

``` r
baseline_only <- anti_join(
  mci_baseline |> distinct(id),
  mci_amyloid |> distinct(id),
  by = "id"
)

amyloid_only <- anti_join(
  mci_amyloid |> distinct(id),
  mci_baseline |> distinct(id),
  by = "id"
)

nrow(baseline_only)
```

    ## [1] 8

``` r
nrow(amyloid_only)
```

    ## [1] 16

``` r
mci_combined <- inner_join(
  mci_baseline,
  mci_amyloid,
  by = "id"
)

mci_combined |>
  summarise(
    n_participants = n_distinct(id),
    n_rows = n()
  )
```

    ## # A tibble: 1 × 2
    ##   n_participants n_rows
    ##            <int>  <int>
    ## 1            471   2355

``` r
write_csv(
  mci_combined,
  "Data/data_mci/mci_combined.csv"
)
```

After excluding four participants whose recorded age of MCI onset was at
or before their baseline age, the cleaned baseline dataset included 479
participants. Of these, 93 developed MCI during follow-up. The average
baseline age was 65.03 years. Among the 210 women in the study, 63 were
APOE4 carriers (30.0%). The longitudinal amyloid dataset contains
measurements at baseline and years 2, 4, 6, and 8. Eight participants in
the cleaned baseline data did not appear in the amyloid data, and 16
participants in the amyloid data did not appear in the cleaned baseline
data. An inner join retained 471 participants and produced 2,355
participant-time observations. The combined dataset was exported to
`Data/data_mci/mci_combined.csv`.
