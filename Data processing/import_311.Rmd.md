311 Heat Complaints: API Verification
================

This file checks the NYC 311 Service Requests API before we download any
record-level data. It confirms the field names, the complaint types
related to heat, the number of HEAT/HOT WATER requests in each heating
season, and how those requests split between heat and hot water.

Data source: [311 Service Requests from 2020 to
Present](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2020-to-Present/erm2-nwe9/about_data)
(NYC Open Data, dataset ID `erm2-nwe9`).

``` r
library(tidyverse)
library(httr)

base_url = "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
retrieval_date = Sys.Date()
```

## Field names

``` r
peek_response = GET(base_url, query = list("$limit" = 5))

status_code(peek_response)
```

    ## [1] 200

``` r
peek_311 = content(peek_response, "parsed")
```

    ## Rows: 5 Columns: 44
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (24): agency, agency_name, complaint_type, descriptor, location_type, i...
    ## dbl   (7): unique_key, incident_zip, bbl, x_coordinate_state_plane, y_coordi...
    ## lgl  (12): closed_date, descriptor_2, facility_type, due_date, resolution_de...
    ## dttm  (1): created_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
names(peek_311)
```

    ##  [1] "unique_key"                     "created_date"                  
    ##  [3] "closed_date"                    "agency"                        
    ##  [5] "agency_name"                    "complaint_type"                
    ##  [7] "descriptor"                     "descriptor_2"                  
    ##  [9] "location_type"                  "incident_zip"                  
    ## [11] "incident_address"               "street_name"                   
    ## [13] "cross_street_1"                 "cross_street_2"                
    ## [15] "intersection_street_1"          "intersection_street_2"         
    ## [17] "address_type"                   "city"                          
    ## [19] "landmark"                       "facility_type"                 
    ## [21] "status"                         "due_date"                      
    ## [23] "resolution_description"         "resolution_action_updated_date"
    ## [25] "community_board"                "council_district"              
    ## [27] "police_precinct"                "bbl"                           
    ## [29] "borough"                        "x_coordinate_state_plane"      
    ## [31] "y_coordinate_state_plane"       "open_data_channel_type"        
    ## [33] "park_facility_name"             "park_borough"                  
    ## [35] "vehicle_type"                   "taxi_company_borough"          
    ## [37] "taxi_pick_up_location"          "bridge_highway_name"           
    ## [39] "bridge_highway_direction"       "road_ramp"                     
    ## [41] "bridge_highway_segment"         "latitude"                      
    ## [43] "longitude"                      "location"

## Complaint types related to heat

``` r
heat_types =
  GET(base_url, query = list(
    "$select" = "complaint_type, count(*) AS n",
    "$where"  = "upper(complaint_type) like '%HEAT%' AND created_date >= '2020-10-01T00:00:00'",
    "$group"  = "complaint_type")) |>
  content("parsed")
```

    ## Rows: 3 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): complaint_type
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
heat_types
```

    ## # A tibble: 3 × 2
    ##   complaint_type             n
    ##   <chr>                  <dbl>
    ## 1 Heat/Hot Water          1814
    ## 2 HEAT/HOT WATER       1621983
    ## 3 Non-Residential Heat    6378

## HEAT/HOT WATER requests by heating season

Each heating season runs from October 1 through May 31. The end date is
set to June 1 with a strict `<` so that all of May 31 is included.

``` r
season_bounds =
  tibble(
    season = str_c(2020:2025, "-", 21:26),
    start  = str_c(2020:2025, "-10-01T00:00:00"),
    end    = str_c(2021:2026, "-06-01T00:00:00"))

count_heat = function(start, end) {

  where_clause =
    str_c("complaint_type = 'HEAT/HOT WATER' AND created_date >= '", start,
          "' AND created_date < '", end, "'")

  GET(base_url, query = list("$select" = "count(*) AS n", "$where" = where_clause)) |>
    content("parsed") |>
    pull(n)
}

season_counts =
  season_bounds |>
  mutate(n_api = map2_dbl(start, end, count_heat))
```

    ## Rows: 1 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 1 Columns: 1
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
season_counts
```

    ## # A tibble: 6 × 4
    ##   season  start               end                  n_api
    ##   <chr>   <chr>               <chr>                <dbl>
    ## 1 2020-21 2020-10-01T00:00:00 2021-06-01T00:00:00 178420
    ## 2 2021-22 2021-10-01T00:00:00 2022-06-01T00:00:00 222676
    ## 3 2022-23 2022-10-01T00:00:00 2023-06-01T00:00:00 225132
    ## 4 2023-24 2023-10-01T00:00:00 2024-06-01T00:00:00 234444
    ## 5 2024-25 2024-10-01T00:00:00 2025-06-01T00:00:00 280427
    ## 6 2025-26 2025-10-01T00:00:00 2026-06-01T00:00:00 341824

## Heat versus hot water in 2025-26

``` r
descriptor_2526 =
  GET(base_url, query = list(
    "$select" = "descriptor, count(*) AS n",
    "$where"  = "complaint_type = 'HEAT/HOT WATER' AND created_date >= '2025-10-01T00:00:00' AND created_date < '2026-06-01T00:00:00'",
    "$group"  = "descriptor")) |>
  content("parsed")
```

    ## Rows: 2 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): descriptor
    ## dbl (1): n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
descriptor_2526
```

    ## # A tibble: 2 × 2
    ##   descriptor           n
    ##   <chr>            <dbl>
    ## 1 APARTMENT ONLY  116854
    ## 2 ENTIRE BUILDING 224970

## Follow-up checks

The first run showed that `descriptor` records the scope of a complaint
(`APARTMENT ONLY` or `ENTIRE BUILDING`), not whether it is about heat or
hot water. It also showed a second spelling, `Heat/Hot Water`. The
checks below count complaints case-insensitively and look at
`descriptor_2`, which may hold the heat versus hot water detail.

``` r
count_heat_upper = function(start, end) {

  where_clause =
    str_c("upper(complaint_type) = 'HEAT/HOT WATER' AND created_date >= '", start,
          "' AND created_date < '", end, "'")

  GET(base_url, query = list("$select" = "count(*) AS n", "$where" = where_clause)) |>
    content("parsed", show_col_types = FALSE) |>
    pull(n)
}

season_counts =
  season_counts |>
  mutate(
    n_api_upper = map2_dbl(start, end, count_heat_upper),
    n_other_case = n_api_upper - n_api)

season_counts |>
  select(season, n_api, n_api_upper, n_other_case)
```

    ## # A tibble: 6 × 4
    ##   season   n_api n_api_upper n_other_case
    ##   <chr>    <dbl>       <dbl>        <dbl>
    ## 1 2020-21 178420      178420            0
    ## 2 2021-22 222676      222676            0
    ## 3 2022-23 225132      225132            0
    ## 4 2023-24 234444      235654         1210
    ## 5 2024-25 280427      280427            0
    ## 6 2025-26 341824      341824            0

``` r
count_descriptor_2 = function(start, end) {

  where_clause =
    str_c("upper(complaint_type) = 'HEAT/HOT WATER' AND created_date >= '", start,
          "' AND created_date < '", end, "'")

  GET(base_url, query = list(
    "$select" = "descriptor_2, count(*) AS n",
    "$where"  = where_clause,
    "$group"  = "descriptor_2")) |>
    content("parsed", show_col_types = FALSE)
}

descriptor_2_counts =
  season_bounds |>
  mutate(counts = map2(start, end, count_descriptor_2)) |>
  select(season, counts) |>
  unnest(counts)

descriptor_2_counts
```

    ## # A tibble: 22 × 3
    ##    season  descriptor_2                  n
    ##    <chr>   <chr>                     <dbl>
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
    ## # ℹ 12 more rows

## Provenance

Queries were run on 2026-11-25. Across the six heating seasons from
2020-21 to 2025-26, the API returned 1,484,133 HEAT/HOT WATER service
requests (case-insensitive match).
