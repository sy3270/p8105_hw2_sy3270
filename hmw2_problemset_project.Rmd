---
title: "hmw2_problems"
author: "shuai yuan"
date: "2024-09-29"
output: github_document
---

## Initialization
```{r setup, include=FALSE}

library(tidyverse)
library(readxl)
library(janitor)

```

## Problem 1
Import subway E&T data and clean data, In this section, the NYC Subway data is imported, and relevant columns are selected. The `Entry` column is converted from "YES"/"NO" to `TRUE`/`FALSE`, and duplicate rows are removed for cleaner analysis.

## Load and Clean Data

```{r }

subway_data <- read_csv("./NYC_Transit_Subway_Entrance_And_Exit_Data.csv")

cleaned_subway_data <- subway_data %>%
  select(Line, `Station Name`, `Station Latitude`, `Station Longitude`, 
         Route1, Route2, Route3, Route4, Route5, 
         Entry, Vending, `Entrance Type`, ADA) %>%
  mutate(Entry = ifelse(Entry == "YES", TRUE, FALSE)) %>%
  distinct()

dim(cleaned_subway_data)


```
## Q1: How many distinct stations are there (by station name and line)?

```{r}
num_distinct_stations <- cleaned_subway_data %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of distinct stations:", num_distinct_stations, "\n")


```
## Q2: How many stations are ADA compliant?
```{r }
num_ada_compliant_stations <- cleaned_subway_data %>%
  filter(ADA == TRUE) %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of ADA compliant stations:", num_ada_compliant_stations, "\n")


```
## Q3: What proportion of entrances/exits without vending allow entry?
```{r }
proportion_no_vending_allow_entry <- cleaned_subway_data %>%
  filter(Vending == "NO") %>%
  summarise(proportion = mean(Entry)) %>%
  pull(proportion)

cat("Proportion of no-vending entrances that allow entry:", proportion_no_vending_allow_entry, "\n")

```


## Reformat data: combine all route columns into a single list
```{r }

cleaned_subway_data <- cleaned_subway_data %>%
  unite("Routes", Route1:Route5, sep = ",", na.rm = TRUE, remove = FALSE) %>%
  separate_rows(Routes, sep = ",")

```
## Q4 How many distinct stations serve the A train?
```{r }
num_a_train_stations <- cleaned_subway_data %>%
  filter(Routes == "A") %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of stations serving the A train:", num_a_train_stations, "\n")
```
# Q5: Of the stations that serve the A train, how many are ADA compliant?

```{r }
num_ada_compliant_a_train_stations <- cleaned_subway_data %>%
  filter(Routes == "A", ADA == TRUE) %>%
  distinct(Line, `Station Name`) %>%
  nrow()

cat("Number of ADA compliant stations serving the A train:", num_ada_compliant_a_train_stations, "\n")

```
## show results


```{r pressure, echo=FALSE}
cat("Number of distinct stations:", num_distinct_stations, "\n")
cat("Number of ADA compliant stations:", num_ada_compliant_stations, "\n")
cat("Proportion of no-vending entrances that allow entry:", proportion_no_vending_allow_entry, "\n")
cat("Number of stations serving the A train:", num_a_train_stations, "\n")
cat("Number of ADA compliant stations serving the A train:", num_ada_compliant_a_train_stations, "\n")
```
In Problem 1, we successfully imported, cleaned, and analyzed the NYC Subway Entrance & Exit dataset. By filtering and transforming the data, we identified that there are a significant number of distinct subway stations across various lines. We also examined the accessibility of these stations and found the number of ADA-compliant stations. Additionally, we calculated the proportion of no-vending entrances that still allow entry, providing insights into the availability of entry options for passengers. 

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

## Problem 2

We import data for "Mr. Trash Wheel", "Professor Trash Wheel", and "Gwynnda Trash Wheel". After cleaning the data, we perform specific analyses to find insights on the trash collection efforts by these initiatives.



## Import data of Mr trash wheel
```{r}
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

```{r}
head(mr_trash_wheel_df, 5)
```

## Import and Clean Data for Professor Trash Wheel


```{r}

prof_trash_wheel_df <- read_excel("./202409 Trash Wheel Collection Data.xlsx",
                                  sheet = "Professor Trash Wheel",
                                  range = "A2:M108",
                                  na = c("NA", "", ".")) %>%
  janitor::clean_names() %>%
  filter(!is.na(dumpster)) %>%
  mutate(trash_wheel = "prof_trash_wheel")

head(prof_trash_wheel_df, 5)


```


## Import and Clean Data for Gwynnda Trash Wheel


```{r}

gwynnda_trash_wheel_df <- read_excel("./202409 Trash Wheel Collection Data.xlsx",
                                     sheet = "Gwynnda Trash Wheel",
                                     range = "A2:L157",
                                     na = c("NA", "", ".")) %>%
  janitor::clean_names() %>%
  filter(!is.na(dumpster)) %>%
  mutate(trash_wheel = "gwynnda_trash_wheel")

head(gwynnda_trash_wheel_df, 5)

```
## Combine All Trash Wheel Data

The trash collection data for "Mr. Trash Wheel", "Professor Trash Wheel", and "Gwynnda Trash Wheel" are imported and combined into a single tidy dataframe for further analysis.

```{r}

trash_wheel_tidy <- bind_rows(mr_trash_wheel_df, prof_trash_wheel_df, gwynnda_trash_wheel_df) %>%
  janitor::clean_names() %>%
  relocate(trash_wheel)


```


##Calculate Total Weight of Professor Trash Wheel and Cigarette Butts Collected by Gwynnda in June 2022


```{r}

total_weight = sum(trash_wheel_tidy$weight_tons[trash_wheel_tidy$trash_wheel == "prof_trash_wheel"],
                   na.rm = TRUE)

cigarette_butt = sum(trash_wheel_tidy$cigarette_butts[
  trash_wheel_tidy$trash_wheel == "gwynnda_trash_wheel" &
  trash_wheel_tidy$month == "June" &
  trash_wheel_tidy$year == 2022],
  na.rm = TRUE)


```

##Results

The total weight of trash collected by Professor Trash Wheel is r **total_weight tons**, and the total number of cigarette butts collected by Gwynnda Trash Wheel in June of 2022 is r **cigarette_butt counts**. 

### Problem 2 detailss:
- **`clean_names()`** from the `janitor` package is used to clean the column names.
- **`read_excel()`** specifies the sheet, range, and missing value handling for each Trash Wheel dataset.
- We filter out rows where `dumpster` is `NA`, and the `mutate()` function is used to handle numeric transformations (e.g., rounding sports balls, converting year to numeric).
- The datasets are combined with `bind_rows()` and then cleaned again for consistency.


#Problem 3

In this section, we analyze data from the Great British Bake Off (GBBO). We import, clean, and merge data about bakers, bakes, and results, then answer questions about winners, star bakers, and viewership statistics.



## Import and clean 

We start by importing and cleaning the three datasets: bakers, bakes, and results. This involves handling column names, fixing formatting issues, and ensuring that data is ready for merging.


```{r}

bakers_df = 
  read_csv("./gbb_datasets/bakers.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  separate(
    baker_name, into = c("baker", "last_name"), sep = " "
  )
head(bakers_df, 5)

bakes_df = 
  read_csv("./gbb_datasets/bakes.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  mutate(
    baker = replace(baker, baker == '"Jo"', "Jo")  
  )
head(bakes_df, 5)

results_df = 
  read_csv("./gbb_datasets/results.csv", skip = 2, na = c("NA", "", ".")) |>
  janitor::clean_names()
head(results_df, 5)

```

## merging the datasets

```{r}

baker_show_df = left_join(results_df, bakers_df, by = c("baker", "series")) |>
  left_join(bakes_df, by = c("baker", "series", "episode")) |>
  relocate(baker, last_name, baker_age, baker_occupation, hometown, signature_bake, show_stopper) |>
  arrange(baker) |>
  mutate(result = str_to_lower(result)) |>
  filter(!is.na(result))  
head(baker_show_df, 5)

write_csv(baker_show_df, "./gbb_datasets/baker_show.csv")


```



## Star Baker and Winners from Seasons 5 to 10

```{r}

baker_winner = 
  baker_show_df |>
  filter(series >= 5 & series <= 10) |>
  filter(result == "winner" | result == "star baker") |>
  relocate(series, episode, result) |>
  arrange(series, episode)

head(baker_winner, 5)


```

## 5. Import Viewership Data


```{r}
viewers_df = 
  read_csv("./gbb_datasets/viewers.csv", na = c("NA", "", ".")) |>
  janitor::clean_names()

head(viewers_df, 10)



```
## Average Viewership for Seasons 1 and 5


```{r}

session_1 = round(mean(viewers_df$series_1, na.rm = TRUE), 2)
session_5 = round(mean(viewers_df$series_5, na.rm = TRUE), 2)
session_1
session_5


```

##conclusion

The average viewership in Season 1 was r session_1 million, while the average viewership in Season 5 was r session_5 million. This indicates a significant increase in popularity over the seasons.
