ACS 5-Year Estimates by ZCTA: Income, Poverty, and Renters
================

This file reads neighborhood characteristics for New York City ZIP Code
Tabulation Areas (ZCTAs) from the 2020-2024 American Community Survey
(ACS) 5-year estimates. RQ2 uses them to compute complaint rates per
renter household; RQ3 uses median household income to group complaints
into income quintiles.

The tables were downloaded by hand from
[data.census.gov](https://data.census.gov/), which needs no API key. For
each table below we chose the product “2024: ACS 5-Year Estimates
Detailed Tables”, geography “5-Digit ZIP Code Tabulation Area” within
New York State, and downloaded the CSV. The `...-Data.csv` files are
saved in `data/acs/` and committed to the repository, so anyone can knit
this file without downloading them again.

| Table | Variables used | Meaning |
|----|----|----|
| B01003 | B01003_001E | Total population |
| B25003 | B25003_001E, B25003_003E | Occupied housing units; renter-occupied units |
| B19013 | B19013_001E | Median household income (2024 dollars) |
| B17001 | B17001_001E, B17001_002E | People whose poverty status is known; below poverty |

Building age is not taken from the ACS: table B25035 is not available
for ZCTAs on data.census.gov, and PLUTO gives the year built of every
building, which RQ2 can summarize by ZIP code.

``` r
library(tidyverse)

knitr::opts_chunk$set(message = FALSE, warning = FALSE)

acs_dir = "data/acs"
processed_dir = "data/processed"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
```

## Read the downloaded tables

In each file the first row holds variable codes (for example
`B19013_001E`) and the second row holds long labels, so the second row
is dropped. Only estimate columns (ending in `E`) are kept;
margin-of-error columns (ending in `M`) are not used.

``` r
acs_files = list.files(acs_dir, pattern = "-Data\\.csv$", full.names = TRUE)

acs_files
```

    ## [1] "data/acs/ACSDT5Y2024.B01003-Data.csv"
    ## [2] "data/acs/ACSDT5Y2024.B17001-Data.csv"
    ## [3] "data/acs/ACSDT5Y2024.B19013-Data.csv"
    ## [4] "data/acs/ACSDT5Y2024.B25003-Data.csv"

``` r
read_acs_table = function(path) {

  read_csv(path, col_types = cols(.default = col_character())) |>
    rename(GEO_ID = 1) |>             # first column, in case of a byte-order mark
    slice(-1) |>
    select(GEO_ID, matches("^B\\d{5}_\\d{3}E$"))
}

acs_raw =
  acs_files |>
  map(read_acs_table) |>
  reduce(left_join, by = "GEO_ID")

acs_vars = c(
  population        = "B01003_001E",
  occupied_hh       = "B25003_001E",
  renter_hh         = "B25003_003E",
  median_income     = "B19013_001E",
  poverty_universe  = "B17001_001E",
  poverty_below     = "B17001_002E")

# Stop if a table is missing
acs_vars[!acs_vars %in% names(acs_raw)]   # should be empty
```

    ## named character(0)

``` r
stopifnot(all(acs_vars %in% names(acs_raw)))

nrow(acs_raw)
```

    ## [1] 1824

## Clean

`GEO_ID` ends with the 5-digit ZCTA (for example `860Z200US10001`).
Estimates are stored as text: top-coded values such as `250,000+` are
read as 250000, and values the Census could not compute (shown as `-` or
similar symbols) become missing.

New York City ZCTAs start with 100-104 (Manhattan, Staten Island, the
Bronx), 111-114 and 116 (Queens, Brooklyn), plus 11004 and 11005 in
eastern Queens. Other ZCTAs in New York State are dropped.

``` r
nyc_prefixes = c("100", "101", "102", "103", "104", "111", "112", "113", "114", "116")

acs_zcta =
  acs_raw |>
  mutate(zcta = str_sub(GEO_ID, -5)) |>
  rename(all_of(acs_vars)) |>
  mutate(across(all_of(names(acs_vars)), \(x) suppressWarnings(parse_number(x)))) |>
  mutate(across(all_of(names(acs_vars)), \(x) if_else(x < 0, NA_real_, x))) |>
  filter(str_sub(zcta, 1, 3) %in% nyc_prefixes | zcta %in% c("11004", "11005")) |>
  mutate(
    poverty_rate = poverty_below / poverty_universe,
    renter_share = renter_hh / occupied_hh) |>
  select(
    zcta, population, occupied_hh, renter_hh, renter_share,
    median_income, poverty_rate) |>
  arrange(zcta)

stopifnot(n_distinct(acs_zcta$zcta) == nrow(acs_zcta))
```

## Checks

``` r
acs_zcta |>
  summarize(
    n_zcta = n(),
    n_missing_income = sum(is.na(median_income)),
    n_small = sum(renter_hh < 100, na.rm = TRUE),
    total_population = sum(population, na.rm = TRUE),
    total_renter_hh = sum(renter_hh, na.rm = TRUE)) |>
  knitr::kable(format.args = list(big.mark = ","))
```

| n_zcta | n_missing_income | n_small | total_population | total_renter_hh |
|-------:|-----------------:|--------:|-----------------:|----------------:|
|    211 |               30 |      33 |        8,477,714 |       2,240,528 |

``` r
acs_zcta |>
  select(median_income, poverty_rate, renter_share) |>
  summary()
```

    ##  median_income     poverty_rate      renter_share   
    ##  Min.   : 24086   Min.   :0.00000   Min.   :0.0000  
    ##  1st Qu.: 67754   1st Qu.:0.09632   1st Qu.:0.4842  
    ##  Median : 88568   Median :0.13736   Median :0.6554  
    ##  Mean   : 97299   Mean   :0.16578   Mean   :0.6251  
    ##  3rd Qu.:115360   3rd Qu.:0.20246   3rd Qu.:0.8175  
    ##  Max.   :250000   Max.   :1.00000   Max.   :1.0000  
    ##  NAs    :30       NAs    :27        NAs    :29

The city has about 8.3 million people and about 2.3 million renter
households, so the totals above should be close to those numbers. ZCTAs
with fewer than 100 renter households (parks, airports, single-building
ZIPs) give unstable rates; RQ2 and RQ3 drop them when forming groups.

How many 311 heat complaints can be matched to a ZCTA with income data?

``` r
rq3_complaints = read_rds(file.path(processed_dir, "rq3_complaints.rds"))

rq3_complaints |>
  mutate(
    zip = str_sub(incident_zip, 1, 5),
    in_acs = zip %in% acs_zcta$zcta,
    has_income = zip %in% filter(acs_zcta, !is.na(median_income), renter_hh >= 100)$zcta) |>
  summarize(
    pct_in_acs = mean(in_acs),
    pct_with_income = mean(has_income)) |>
  knitr::kable(digits = 4)
```

| pct_in_acs | pct_with_income |
|-----------:|----------------:|
|     0.9998 |          0.9998 |

``` r
# Complaint ZIPs that do not match
rq3_complaints |>
  mutate(zip = str_sub(incident_zip, 1, 5)) |>
  filter(!zip %in% acs_zcta$zcta) |>
  count(zip, sort = TRUE) |>
  head(10) |>
  knitr::kable()
```

| zip   |   n |
|:------|----:|
| NA    | 163 |
| 11001 |  43 |
| 11040 |  23 |
| 10120 |   3 |
| 11241 |   3 |
| 12345 |   3 |
| 10000 |   2 |
| 10118 |   2 |
| 10041 |   1 |
| 10105 |   1 |

## Save

The file is small (one row per ZCTA), so it is committed to the
repository.

``` r
write_csv(acs_zcta, file.path(processed_dir, "acs_zcta.csv"))
```

Source: U.S. Census Bureau, 2020-2024 American Community Survey 5-year
estimates, Detailed Tables B01003, B25003, B19013, and B17001,
downloaded from data.census.gov.
