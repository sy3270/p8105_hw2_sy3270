hmw2_problems
================
shuai yuan
2024-09-29

## Initialization

## Problem 1

Import subway E&T data and clean data

## Load and Clean Data

``` r
# List available files in the working directory
list.files()
```

    ##  [1] "202309 Trash Wheel Collection Data.xlsx"      
    ##  [2] "202409 Trash Wheel Collection Data.xlsx"      
    ##  [3] "data hmw2_dataset"                            
    ##  [4] "gbb_datasets"                                 
    ##  [5] "gpigs.txt"                                    
    ##  [6] "gpigs20.txt"                                  
    ##  [7] "Hmw2_problemset_knit1.html"                   
    ##  [8] "hmw2_problemset_project.Rmd"                  
    ##  [9] "hmw2_problemset.Rmd"                          
    ## [10] "NYC_Transit_Subway_Entrance_And_Exit_Data.csv"
    ## [11] "README.md"                                    
    ## [12] "sy3270_hmw_2.Rproj"

``` r
# Load the dataset
subway_data <- read_csv("./NYC_Transit_Subway_Entrance_And_Exit_Data.csv")
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Select relevant columns and clean data
cleaned_subway_data <- subway_data %>%
  select(Line, `Station Name`, `Station Latitude`, `Station Longitude`, 
         Route1, Route2, Route3, Route4, Route5, 
         Entry, Vending, `Entrance Type`, ADA) %>%
  mutate(Entry = ifelse(Entry == "YES", TRUE, FALSE)) %>%  # Convert 'Entry' to logical values
  distinct()  # Remove duplicates

# Check the dimensions of the cleaned dataset
dim(cleaned_subway_data)
```

    ## [1] 684  13

## Q1: How many distinct stations are there (by station name and line)?

``` r
num_distinct_stations <- cleaned_subway_data %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of distinct stations:", num_distinct_stations, "\n")
```

    ## Number of distinct stations: 465

## Q2: How many stations are ADA compliant?

``` r
num_ada_compliant_stations <- cleaned_subway_data %>%
  filter(ADA == TRUE) %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of ADA compliant stations:", num_ada_compliant_stations, "\n")
```

    ## Number of ADA compliant stations: 84

## Q3: What proportion of entrances/exits without vending allow entry?

``` r
proportion_no_vending_allow_entry <- cleaned_subway_data %>%
  filter(Vending == "NO") %>%
  summarise(proportion = mean(Entry)) %>%
  pull(proportion)

cat("Proportion of no-vending entrances that allow entry:", proportion_no_vending_allow_entry, "\n")
```

    ## Proportion of no-vending entrances that allow entry: 0.3846154

## Reformat data: combine all route columns into a single list

``` r
cleaned_subway_data <- cleaned_subway_data %>%
  unite("Routes", Route1:Route5, sep = ",", na.rm = TRUE, remove = FALSE) %>%
  separate_rows(Routes, sep = ",")
```

## Q4 How many distinct stations serve the A train?

``` r
num_a_train_stations <- cleaned_subway_data %>%
  filter(Routes == "A") %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of stations serving the A train:", num_a_train_stations, "\n")
```

    ## Number of stations serving the A train: 60

# Q5: Of the stations that serve the A train, how many are ADA compliant?

``` r
num_ada_compliant_a_train_stations <- cleaned_subway_data %>%
  filter(Routes == "A", ADA == TRUE) %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of ADA compliant stations serving the A train:", num_ada_compliant_a_train_stations, "\n")
```

    ## Number of ADA compliant stations serving the A train: 17

## show results

    ## Number of distinct stations: 465

    ## Number of ADA compliant stations: 84

    ## Proportion of no-vending entrances that allow entry: 0.3846154

    ## Number of stations serving the A train: 60

    ## Number of ADA compliant stations serving the A train: 17

In Problem 1, we successfully imported, cleaned, and analyzed the NYC
Subway Entrance & Exit dataset. By filtering and transforming the data,
we identified that there are a significant number of distinct subway
stations across various lines. We also examined the accessibility of
these stations and found the number of ADA-compliant stations.
Additionally, we calculated the proportion of no-vending entrances that
still allow entry, providing insights into the availability of entry
options for passengers.

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

## Problem 2

Import data of Mr trash wheel

``` r
mr_trash_wheel_df = 
  read_excel("./202409 Trash Wheel Collection Data.xlsx",
             sheet = "Mr. Trash Wheel",
             range = "A2:N586",
             na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  filter(!is.na(dumpster)) |>
  mutate(
    sports_balls = as.numeric(round(sports_balls, digits = 0)),
    trash_wheel = "mr_trash_wheel",
    year = as.double(year)
  )
```

## show head

``` r
head(mr_trash_wheel_df, 5)
```

    ## # A tibble: 5 × 15
    ##   dumpster month  year date                weight_tons volume_cubic_yards
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ## 1        1 May    2014 2014-05-16 00:00:00        4.31                 18
    ## 2        2 May    2014 2014-05-16 00:00:00        2.74                 13
    ## 3        3 May    2014 2014-05-16 00:00:00        3.45                 15
    ## 4        4 May    2014 2014-05-17 00:00:00        3.1                  15
    ## 5        5 May    2014 2014-05-17 00:00:00        4.06                 18
    ## # ℹ 9 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, sports_balls <dbl>, homes_powered <dbl>, trash_wheel <chr>

## Import and Clean Data for Professor Trash Wheel

``` r
prof_trash_wheel_df <- read_excel("./202409 Trash Wheel Collection Data.xlsx",
                                  sheet = "Professor Trash Wheel",
                                  range = "A2:M108",
                                  na = c("NA", "", ".")) %>%
  janitor::clean_names() %>%
  filter(!is.na(dumpster)) %>%
  mutate(trash_wheel = "prof_trash_wheel")

head(prof_trash_wheel_df, 5)
```

    ## # A tibble: 5 × 14
    ##   dumpster month     year date                weight_tons volume_cubic_yards
    ##      <dbl> <chr>    <dbl> <dttm>                    <dbl>              <dbl>
    ## 1        1 January   2017 2017-01-02 00:00:00        1.79                 15
    ## 2        2 January   2017 2017-01-30 00:00:00        1.58                 15
    ## 3        3 February  2017 2017-02-26 00:00:00        2.32                 18
    ## 4        4 February  2017 2017-02-26 00:00:00        3.72                 15
    ## 5        5 February  2017 2017-02-28 00:00:00        1.45                 15
    ## # ℹ 8 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, plastic_bags <dbl>,
    ## #   wrappers <dbl>, homes_powered <dbl>, trash_wheel <chr>

## Import and Clean Data for Gwynnda Trash Wheel

``` r
gwynnda_trash_wheel_df <- read_excel("./202409 Trash Wheel Collection Data.xlsx",
                                     sheet = "Gwynnda Trash Wheel",
                                     range = "A2:L157",
                                     na = c("NA", "", ".")) %>%
  janitor::clean_names() %>%
  filter(!is.na(dumpster)) %>%
  mutate(trash_wheel = "gwynnda_trash_wheel")

head(gwynnda_trash_wheel_df, 5)
```

    ## # A tibble: 5 × 13
    ##   dumpster month  year date                weight_tons volume_cubic_yards
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>              <dbl>
    ## 1        1 July   2021 2021-07-03 00:00:00        0.93                 15
    ## 2        2 July   2021 2021-07-07 00:00:00        2.26                 15
    ## 3        3 July   2021 2021-07-07 00:00:00        1.62                 15
    ## 4        4 July   2021 2021-07-16 00:00:00        1.76                 15
    ## 5        5 July   2021 2021-07-30 00:00:00        1.53                 15
    ## # ℹ 7 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, plastic_bags <dbl>, wrappers <dbl>,
    ## #   homes_powered <dbl>, trash_wheel <chr>

## Combine All Trash Wheel Data

``` r
trash_wheel_tidy <- bind_rows(mr_trash_wheel_df, prof_trash_wheel_df, gwynnda_trash_wheel_df) %>%
  janitor::clean_names() %>%
  relocate(trash_wheel)
```

\##Calculate Total Weight of Professor Trash Wheel and Cigarette Butts
Collected by Gwynnda in June 2022

``` r
total_weight = sum(trash_wheel_tidy$weight_tons[trash_wheel_tidy$trash_wheel == "prof_trash_wheel"],
                   na.rm = TRUE)

cigarette_butt = sum(trash_wheel_tidy$cigarette_butts[
  trash_wheel_tidy$trash_wheel == "gwynnda_trash_wheel" &
  trash_wheel_tidy$month == "June" &
  trash_wheel_tidy$year == 2022],
  na.rm = TRUE)
```

\##Results

The total weight of trash collected by Professor Trash Wheel is r
**total_weight tons**, and the total number of cigarette butts collected
by Gwynnda Trash Wheel in June of 2022 is r **cigarette_butt counts**.

### Problem 2 detailss:

- **`clean_names()`** from the `janitor` package is used to clean the
  column names.
- **`read_excel()`** specifies the sheet, range, and missing value
  handling for each Trash Wheel dataset.
- We filter out rows where `dumpster` is `NA`, and the `mutate()`
  function is used to handle numeric transformations (e.g., rounding
  sports balls, converting year to numeric).
- The datasets are combined with `bind_rows()` and then cleaned again
  for consistency.

\#Problem 3

## Import and clean

``` r
bakers_df = 
  read_csv("./gbb_datasets/bakers.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  separate(
    baker_name, into = c("baker", "last_name"), sep = " "
  )
```

    ## Rows: 120 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker Name, Baker Occupation, Hometown
    ## dbl (2): Series, Baker Age
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(bakers_df, 5)
```

    ## # A tibble: 5 × 6
    ##   baker  last_name  series baker_age baker_occupation   hometown                
    ##   <chr>  <chr>       <dbl>     <dbl> <chr>              <chr>                   
    ## 1 Ali    Imdad           4        25 Charity worker     Saltley, Birmingham     
    ## 2 Alice  Fevronia       10        28 Geography teacher  Essex                   
    ## 3 Alvin  Magallanes      6        37 Nurse              Bracknell, Berkshire    
    ## 4 Amelia LeBruin        10        24 Fashion designer   Halifax                 
    ## 5 Andrew Smyth           7        25 Aerospace engineer Derby / Holywood, Count…

``` r
# Import and clean the bakes data
bakes_df = 
  read_csv("./gbb_datasets/bakes.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  mutate(
    baker = replace(baker, baker == '"Jo"', "Jo")  # Fix any potential formatting issues with names
  )
```

    ## Rows: 548 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Baker, Signature Bake, Show Stopper
    ## dbl (2): Series, Episode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(bakes_df, 5)
```

    ## # A tibble: 5 × 5
    ##   series episode baker     signature_bake                           show_stopper
    ##    <dbl>   <dbl> <chr>     <chr>                                    <chr>       
    ## 1      1       1 Annetha   Light Jamaican Black Cakewith Strawberr… Red, White …
    ## 2      1       1 David     Chocolate Orange Cake                    Black Fores…
    ## 3      1       1 Edd       Caramel Cinnamon and Banana Cake         N/A         
    ## 4      1       1 Jasminder Fresh Mango and Passion Fruit Hummingbi… N/A         
    ## 5      1       1 Jonathan  Carrot Cake with Lime and Cream Cheese … Three Tiere…

``` r
# Import and clean the results data
results_df = 
  read_csv("./gbb_datasets/results.csv", skip = 2, na = c("NA", "", ".")) |>
  janitor::clean_names()
```

    ## Rows: 1136 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): baker, result
    ## dbl (3): series, episode, technical
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(results_df, 5)
```

    ## # A tibble: 5 × 5
    ##   series episode baker     technical result
    ##    <dbl>   <dbl> <chr>         <dbl> <chr> 
    ## 1      1       1 Annetha           2 IN    
    ## 2      1       1 David             3 IN    
    ## 3      1       1 Edd               1 IN    
    ## 4      1       1 Jasminder        NA IN    
    ## 5      1       1 Jonathan          9 IN

## merging the datasets

``` r
# Merge the datasets
baker_show_df = left_join(results_df, bakers_df, by = c("baker", "series")) |>
  left_join(bakes_df, by = c("baker", "series", "episode")) |>
  relocate(baker, last_name, baker_age, baker_occupation, hometown, signature_bake, show_stopper) |>
  arrange(baker) |>
  mutate(result = str_to_lower(result)) |>
  filter(!is.na(result))  # Ensure that only rows with result are kept
head(baker_show_df, 5)
```

    ## # A tibble: 5 × 11
    ##   baker last_name baker_age baker_occupation  hometown            signature_bake
    ##   <chr> <chr>         <dbl> <chr>             <chr>               <chr>         
    ## 1 Ali   Imdad            25 Charity worker    Saltley, Birmingham Rose and Pist…
    ## 2 Ali   Imdad            25 Charity worker    Saltley, Birmingham Italian Griss…
    ## 3 Ali   Imdad            25 Charity worker    Saltley, Birmingham Coconut, Rasp…
    ## 4 Ali   Imdad            25 Charity worker    Saltley, Birmingham Apple and Gin…
    ## 5 Alice Fevronia         28 Geography teacher Essex               <NA>          
    ## # ℹ 5 more variables: show_stopper <chr>, series <dbl>, episode <dbl>,
    ## #   technical <dbl>, result <chr>

``` r
# Export the merged and cleaned dataset
write_csv(baker_show_df, "./gbb_datasets/baker_show.csv")
```

## Star Baker and Winners from Seasons 5 to 10

``` r
# Filter to get winners and star bakers from Seasons 5 to 10
baker_winner = 
  baker_show_df |>
  filter(series >= 5 & series <= 10) |>
  filter(result == "winner" | result == "star baker") |>
  relocate(series, episode, result) |>
  arrange(series, episode)

# Display the filtered results
head(baker_winner, 5)
```

    ## # A tibble: 5 × 11
    ##   series episode result     baker  last_name baker_age baker_occupation hometown
    ##    <dbl>   <dbl> <chr>      <chr>  <chr>         <dbl> <chr>            <chr>   
    ## 1      5       1 star baker Nancy  Birtwhis…        60 Retired Practic… Barton-…
    ## 2      5       2 star baker Richa… Burr             38 Builder          Mill Hi…
    ## 3      5       3 star baker Luis   Troyano          42 Graphic Designer Poynton…
    ## 4      5       4 star baker Richa… Burr             38 Builder          Mill Hi…
    ## 5      5       5 star baker Kate   Henry            41 Furniture Resto… Brighto…
    ## # ℹ 3 more variables: signature_bake <chr>, show_stopper <chr>, technical <dbl>

## 5. Import Viewership Data

``` r
# Import and clean the viewership data
viewers_df = 
  read_csv("./gbb_datasets/viewers.csv", na = c("NA", "", ".")) |>
  janitor::clean_names()
```

    ## Rows: 10 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): Episode, Series 1, Series 2, Series 3, Series 4, Series 5, Series ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Show the first 10 rows of the viewership data
head(viewers_df, 10)
```

    ## # A tibble: 10 × 11
    ##    episode series_1 series_2 series_3 series_4 series_5 series_6 series_7
    ##      <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1       1     2.24     3.1      3.85     6.6      8.51     11.6     13.6
    ##  2       2     3        3.53     4.6      6.65     8.79     11.6     13.4
    ##  3       3     3        3.82     4.53     7.17     9.28     12.0     13.0
    ##  4       4     2.6      3.6      4.71     6.82    10.2      12.4     13.3
    ##  5       5     3.03     3.83     4.61     6.95     9.95     12.4     13.1
    ##  6       6     2.75     4.25     4.82     7.32    10.1      12       13.1
    ##  7       7    NA        4.42     5.1      7.76    10.3      12.4     13.4
    ##  8       8    NA        5.06     5.35     7.41     9.02     11.1     13.3
    ##  9       9    NA       NA        5.7      7.41    10.7      12.6     13.4
    ## 10      10    NA       NA        6.74     9.45    13.5      15.0     15.9
    ## # ℹ 3 more variables: series_8 <dbl>, series_9 <dbl>, series_10 <dbl>

## Average Viewership for Seasons 1 and 5

``` r
# Calculate average viewership for Season 1 and Season 5
session_1 = round(mean(viewers_df$series_1, na.rm = TRUE), 2)
session_5 = round(mean(viewers_df$series_5, na.rm = TRUE), 2)

# Display the results
session_1
```

    ## [1] 2.77

``` r
session_5
```

    ## [1] 10.04

\##conclusion

The average viewership in Season 1 was r session_1 million, while the
average viewership in Season 5 was r session_5 million. This indicates a
significant increase in popularity over the seasons.
