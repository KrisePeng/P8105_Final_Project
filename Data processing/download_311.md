311 Heat Complaints: Record-Level Download
================

This file downloads every HEAT/HOT WATER service request created during
the six heating seasons from 2020-21 to 2025-26. Each season runs from
October 1 through May 31.

Records are downloaded in pages ordered by `unique_key`. Every page is
checked before it is kept: the request must succeed and return exactly
the expected number of rows. A failed page is retried up to three times,
and the code stops with an error instead of saving incomplete data.

Each season is saved as its own file in `data/raw/`, which is not
committed to GitHub. If a season file already exists, it is read from
disk instead of being downloaded again, so an interrupted run can be
resumed.

``` r
library(tidyverse)
library(httr)

base_url = "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
raw_dir = "data/raw"
page_size = 25000

# Optional NYC Open Data app token, stored outside the repository in ~/.Renviron
app_token = Sys.getenv("NYC_OPEN_DATA_TOKEN")

token_header =
  if (app_token == "") add_headers() else add_headers("X-App-Token" = app_token)

select_cols =
  str_c(
    "unique_key, created_date, closed_date, complaint_type, descriptor, descriptor_2, ",
    "status, resolution_description, resolution_action_updated_date, ",
    "incident_zip, incident_address, bbl, borough, community_board, ",
    "latitude, longitude, location_type, open_data_channel_type")
```

## Expected counts from the API

``` r
season_bounds =
  tibble(
    season = str_c(2020:2025, "-", 21:26),
    start  = str_c(2020:2025, "-10-01T00:00:00"),
    end    = str_c(2021:2026, "-06-01T00:00:00")) |>
  mutate(
    where_clause =
      str_c("upper(complaint_type) = 'HEAT/HOT WATER' AND created_date >= '", start,
            "' AND created_date < '", end, "'"))

count_rows = function(where_clause) {

  GET(base_url, token_header, query = list("$select" = "count(*) AS n", "$where" = where_clause)) |>
    content("parsed", show_col_types = FALSE) |>
    pull(n)
}

season_bounds =
  season_bounds |>
  mutate(n_expected = map_dbl(where_clause, count_rows))

season_bounds |>
  select(season, n_expected)
```

    ## # A tibble: 6 × 2
    ##   season  n_expected
    ##   <chr>        <dbl>
    ## 1 2020-21     178420
    ## 2 2021-22     222676
    ## 3 2022-23     225132
    ## 4 2023-24     235654
    ## 5 2024-25     280427
    ## 6 2025-26     341824

## Download

All columns are read as character so that ZIP codes, BBLs, and dates are
not changed by automatic type guessing. Types are converted in the
cleaning step.

`$limit` and `$offset` are written with
`format(..., scientific = FALSE)`. Without it, R converts numbers such
as 100000 to `1e+05` when building the request URL, and the API rejects
those pages.

``` r
get_page = function(offset, where_clause, n_expected) {

  n_should = min(page_size, n_expected - offset)

  for (attempt in 1:3) {

    Sys.sleep(2 * attempt)

    response =
      GET(base_url, token_header, timeout(300), query = list(
        "$select" = select_cols,
        "$where"  = where_clause,
        "$order"  = "unique_key",
        "$limit"  = format(page_size, scientific = FALSE),
        "$offset" = format(offset, scientific = FALSE)))

    if (status_code(response) == 200) {

      page = content(response, "parsed", col_types = cols(.default = col_character()))

      if (nrow(page) == n_should && all(!is.na(page$unique_key))) {
        return(page)
      }
    }

    message("Offset ", format(offset, scientific = FALSE), ": attempt ", attempt,
            " failed (status ", status_code(response), ")")
  }

  stop("Page at offset ", format(offset, scientific = FALSE), " failed after 3 attempts.")
}

download_season = function(season_name) {

  bounds = season_bounds |> filter(season == season_name)
  season_path = str_c(raw_dir, "/311_heat_", season_name, ".csv.gz")

  if (file.exists(season_path)) {

    season_data = read_csv(season_path, col_types = cols(.default = col_character()))

  } else {

    offsets = seq(0, bounds$n_expected - 1, by = page_size)

    season_data =
      map(offsets, get_page, where_clause = bounds$where_clause, n_expected = bounds$n_expected) |>
      bind_rows() |>
      mutate(season = season_name) |>
      relocate(season)

    write_csv(season_data, season_path)
  }

  season_data
}
```

``` r
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

heat_raw =
  map(season_bounds$season, download_season) |>
  bind_rows()
```

## Completeness checks

``` r
download_check =
  heat_raw |>
  count(season, name = "n_downloaded") |>
  left_join(select(season_bounds, season, n_expected), by = "season") |>
  mutate(match = n_downloaded == n_expected)

download_check
```

    ## # A tibble: 6 × 4
    ##   season  n_downloaded n_expected match
    ##   <chr>          <int>      <dbl> <lgl>
    ## 1 2020-21       178420     178420 TRUE 
    ## 2 2021-22       222676     222676 TRUE 
    ## 3 2022-23       225132     225132 TRUE 
    ## 4 2023-24       235654     235654 TRUE 
    ## 5 2024-25       280427     280427 TRUE 
    ## 6 2025-26       341824     341824 TRUE

``` r
heat_raw |>
  summarize(
    n_rows = n(),
    n_unique_keys = n_distinct(unique_key),
    n_missing_keys = sum(is.na(unique_key)))
```

    ## # A tibble: 1 × 3
    ##    n_rows n_unique_keys n_missing_keys
    ##     <int>         <int>          <int>
    ## 1 1484133       1484133              0

``` r
heat_raw |>
  count(complaint_type)
```

    ## # A tibble: 2 × 2
    ##   complaint_type       n
    ##   <chr>            <int>
    ## 1 HEAT/HOT WATER 1482923
    ## 2 Heat/Hot Water    1210

``` r
heat_raw |>
  count(season, descriptor_2) |>
  print(n = 30)
```

    ## # A tibble: 22 × 3
    ##    season  descriptor_2                  n
    ##    <chr>   <chr>                     <int>
    ##  1 2020-21 HEAT ON IN SUMMER             2
    ##  2 2020-21 NO HEAT                  108944
    ##  3 2020-21 NO HEAT AND NO HOT WATER  47618
    ##  4 2020-21 NO HOT WATER              21856
    ##  5 2021-22 NO HEAT                  134454
    ##  6 2021-22 NO HEAT AND NO HOT WATER  64085
    ##  7 2021-22 NO HOT WATER              24137
    ##  8 2022-23 NO HEAT                  139901
    ##  9 2022-23 NO HEAT AND NO HOT WATER  60147
    ## 10 2022-23 NO HOT WATER              25084
    ## 11 2023-24 NO HEAT                  142498
    ## 12 2023-24 NO HEAT AND NO HOT WATER  64151
    ## 13 2023-24 NO HOT WATER              27795
    ## 14 2023-24 No Heat                     174
    ## 15 2023-24 No Heat and No Hot Water    170
    ## 16 2023-24 No Hot Water                866
    ## 17 2024-25 NO HEAT                  174076
    ## 18 2024-25 NO HEAT AND NO HOT WATER  74624
    ## 19 2024-25 NO HOT WATER              31727
    ## 20 2025-26 NO HEAT                  211741
    ## 21 2025-26 NO HEAT AND NO HOT WATER  91693
    ## 22 2025-26 NO HOT WATER              38390

## Provenance

Downloaded from the NYC Open Data API (`erm2-nwe9`). The season files in
`data/raw/` were first downloaded on 2026-11-25 (update this date if
they are re-downloaded); this report was knit on 2026-11-25. The data
contain 1,484,133 records across six heating seasons.
