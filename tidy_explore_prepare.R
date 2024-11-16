library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(readr)


### PISA analysis ###

# We extract series info from series code

PISA_data <- read_csv("data/PISA_results/PISA_stats.csv", na = c("", "..")) |>
  janitor::clean_names() |>
  janitor::remove_empty(which = c("rows", "cols")) |>
  filter(!if_all(matches(r"(x\d+_yr\d+)"), is.na)) |>
  separate_wider_regex(
    series_code,
    c(
      r"(LO.PISA.)",
      subject = r"(MAT|SCI)",
      r"(\.)",
      level = r"(\d[AB]|\d)",
      r"(\.)",
      gender = ".*"
    ),
    too_few = "align_start"
  ) |>
  mutate(gender = gender |> replace_na("Both")) |> 
  mutate(across(!where(is.numeric), function(x)
    fct(x))) |> 
  select(!c(country_code, series_name))

### Add continent info to data

continent_data <- read_csv("data/continents/continents.csv") |>
  janitor::clean_names() |>
  select(entity, continent)
  
PISA_data <- PISA_data |> 
  left_join(continent_data, by = join_by(country_name == entity))

# Some entities are missing their continents

PISA_data |>
  filter(is.na(continent)) |>
  distinct(country_name)

# Only OECD is not a distinct region, others can be added manually

PISA_data <- PISA_data |>
  mutate(continent = case_match(
    country_name,
    c(
      "Brunei Darussalam",
      "Hong Kong SAR, China",
      "Korea, Rep.",
      "Macao SAR, China",
      "Russian Federation",
      "Kyrgyz Republic"
    ) ~ "Asia",
    c("Czech Republic", "Slovak Republic") ~ "Europe",
    .default = continent
  )) |>
  mutate(continent = fct(continent))

### Compare Math Proficiency Level 0 in 2018 between genders

PISA_data |>
  filter(subject == "MAT" & level == 0 & gender != "Both") |>
  select(country_name, x2018_yr2018, gender) |>
  pivot_wider(names_from = gender, values_from = x2018_yr2018) |>
  filter(!if_all(!country_name, is.na)) |>
  mutate(gender_diff = FE - MA) |>
  mutate(country_name = country_name |> fct_reorder(gender_diff)) |>
  pivot_longer(
    !c(country_name, gender_diff),
    names_to = "Gender",
    values_to = "Proportion"
  ) |>
  ggplot(aes(x = country_name, y = Proportion, color = Gender)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 40)) +
  labs(x = "Country", y = "Proportion below level 0 in math proficiency")

### Examine Finland's Math proficiency trends by gender

PISA_data |>
  filter(country_name == "Finland" &
           gender != "Both" & subject == "MAT") |>
  pivot_longer(matches(r"(x\d+_yr\d+)"),
               names_to = "year",
               values_to = "proportion") |>
  mutate(year = parse_number(year)) |>
  mutate(level = level |> fct_reorder2(year, proportion)) |>
  ggplot(aes(x = year, y = proportion)) +
  geom_line(aes(group = level, color = level)) +
  facet_wrap(vars(gender)) + 
  labs(x = "Year", y = "Proportion")

### Examine Math proficiency trends aggregated over continents

PISA_data |> 
  filter(gender == "Both" & subject == "MAT") |> 
  pivot_longer(matches(r"(x\d+_yr\d+)"),
               names_to = "year",
               values_to = "proportion") |> 
  mutate(year = parse_number(year)) |> 
  filter(n_distinct(country_name) > 5, .by = continent) |>
  summarise(med_prop = median(proportion, na.rm = T), .by = c(continent, level, year)) |> 
  ggplot(aes(x = year, y = med_prop)) +
  geom_line(aes(group = level, color = level)) + 
  facet_wrap(vars(continent)) +
  labs(x = "Year", y = "Proportion")
  
### Prepare PISA Math. Prof. Level 0 in 2018 data for regression ###

PISA_math_data_2018 <- PISA_data |>
  filter(subject == "MAT" & level == 0 & gender == "Both") |> 
  select(country_name, continent, x2018_yr2018) |>
  filter(!is.na(x2018_yr2018)) |> 
  filter(country_name != "OECD members") |> 
  rename(MAT.0.Both = x2018_yr2018)

covariate_file_names <- list.files("data/Covariate_csv", full.names = T)

covariate_data <- read_csv(covariate_file_names, na = c("", "..")) |>
  janitor::clean_names() |>
  janitor::remove_empty(which = c("rows", "cols")) |>
  filter(!if_all(matches(r"(x\d+_yr\d+)"), is.na))

covariate_codes <- covariate_data |>
  distinct(series_name, series_code)

covariate_data_2018 <- covariate_data |>
  select(country_name, series_code, x2018_yr2018) |>
  filter(!is.na(x2018_yr2018)) |>
  pivot_wider(names_from = series_code, values_from = x2018_yr2018)

math_covar_data_2018 <- left_join(PISA_math_data_2018, covariate_data_2018, by = join_by(country_name))

# Calculate median response and obs. count for each continent

summ_per_cont <- group_by(math_covar_data_2018, continent, .drop = F) |>
  summarise(median = median(MAT.0.Both), n = n())

summ_per_cont |>
  mutate(continent = continent |> fct_reorder(n)) |>
  ggplot(aes(x = continent, y = n)) +
  geom_col() +
  labs(x = "Continent", y = "# of participating regions")

# To avoid overfitting, lump the smallest continent groups together

math_covar_data_2018 <- math_covar_data_2018 |> 
  mutate(continent = fct_lump_min(continent, 5))

# Check the amount of missing values for each feature

summarise(math_covar_data_2018, across(everything(), function(x)
  mean(is.na(x)))) |>
  tidyr::pivot_longer(everything()) |>
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.1,
                 fill = "white",
                 color = "black") +
  labs(x = "Prop. of missing values", y = "Count")
