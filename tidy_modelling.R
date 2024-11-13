library(ggplot2)
library(dplyr)
library(tidyr)

PISA_code <- "LO.PISA.MAT.0"

cols_to_select <- c("country_name", "series_code", "x2018_yr2018")

PISA_data <- readr::read_csv("data/PISA_results/PISA_stats.csv", na = c("", "..")) |>
  janitor::clean_names() |>
  janitor::remove_empty(which = c("rows", "cols")) |>
  select(all_of(cols_to_select)) |>
  filter(!is.na(x2018_yr2018)) |>
  filter(series_code == PISA_code) |>
  pivot_wider(names_from = series_code, values_from = x2018_yr2018)

covariate_file_names <- list.files("data/Covariate_csv", full.names = T)

covariate_data <- readr::read_csv(covariate_file_names, na = c("", "..")) |>
  janitor::clean_names() |>
  janitor::remove_empty(which = c("rows", "cols")) |>
  select(all_of(cols_to_select)) |>
  filter(!is.na(x2018_yr2018)) |>
  pivot_wider(names_from = series_code, values_from = x2018_yr2018)

continent_data <- readr::read_csv("data/continents/continents.csv") |>
  janitor::clean_names() |>
  select(entity, continent)

full_data <- left_join(PISA_data, covariate_data, by = join_by(country_name)) |>
  left_join(continent_data, by = join_by(country_name == entity))

# Some entities are missing their continents

full_data |>
  filter(is.na(continent)) |>
  select(country_name)

# Only OECD is not a distinct region, others can be added manually

full_data <- filter(full_data, !(country_name == "OECD members")) |>
  mutate(continent = case_match(
    country_name,
    c(
      "Brunei Darussalam",
      "Hong Kong SAR, China",
      "Korea, Rep.",
      "Macao SAR, China",
      "Russian Federation"
    ) ~ "Asia",
    c("Czech Republic", "Slovak Republic") ~ "Europe",
    .default = continent
  )) |>
  mutate(continent = factor(continent))
