library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)
library(forcats)

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')

all_countries_data <- all_countries |> 
  clean_names()

country_regions_data <- country_regions |> 
  clean_names()

# Do a join to practice joining

# are there any missing country_name?

country_regions_data |> 
  filter(is.na(country_name)) |> 
  count()

# No, all country_names are used


# Test all region_code have one region_name

country_regions_data |> 
  distinct(region_code, region_name)

# There is one duplicate. Look into it 

country_regions_data |> 
  filter(region_code == "AM_C") |> 
  count(region_name)

# Okay, after looking at the below data, I will mutate the 
# region_code variable so that there are distinct values.
# All the other variables look reasonable.

country_regions_data_joining <- country_regions_data |> 
  select(region_code,
         region_name,
         country_name,
         country_iso3) |> 
  mutate(region_code = ifelse(region_name == "Central America",
                              "AM_CE",
                              region_code))



joined_data <- all_countries_data |> 
  left_join(country_regions_data_joining,
            by = "country_iso3")


# Check when region_code.x does not equal region_code.y

joined_data |> 
  filter(region_code.x != region_code.y) |> 
  distinct(country_name)

# looks like it's our Central American countries. let's
# remove region_code.x

joined_data <- joined_data |> 
  select(-region_code.x)

final_data <- joined_data |> 
  select(country_name,
         region_name,
         category,
         subcategory,
         population:uncertainty_combined) |> 
  arrange(country_name,
          category,
          subcategory) |> 
  rename(subcategory_hours_per_day = hours_per_day_combined) |> 
  group_by(country_name, category) |> 
  mutate(category_hours_per_day = sum(subcategory_hours_per_day),
         .after = population) |> 
  ungroup()

final_data |> 
  tabyl(category)

# Cool categories!

# I am going to compare categories between the UAE and Micronesia

final_filtered_data <- final_data |> 
  filter(country_name %in% c("Micronesia (Federated States of)",
                             "United Arab Emirates")) |> 
  distinct(country_name, category, .keep_all = TRUE)


final_filtered_data |> 
  mutate(category = category |> 
           as.factor() |> 
           fct_reorder(category_hours_per_day)) |> 
  ggplot() +
  geom_col(aes(x = category,
               y = category_hours_per_day,
               fill = country_name),
           position = "dodge2") +
  labs(x = "Category",
       y = "Hours per day",
       fill = "Country name") +
  coord_flip()

pivot_wider(names_from = station, values_from = seen)

matrix_thingy <- final_data |> 
  distinct(country_name,
         category,
         category_hours_per_day)


# This stack overflow answer 
# https://stackoverflow.com/questions/67714078/how-to-get-the-pairwise-difference-of-all-values-within-uneven-categories-in-r


country_pair_diff <- matrix_thingy |> 
  group_by(category) |> 
  reframe(result = combn(seq_along(category_hours_per_day), 2, function(i)
    list(difference = diff(category_hours_per_day[i]),  #The difference
         country_pair = paste0(country_name[i], collapse = '-')), # The pairs
    simplify = FALSE)) |> 
  unnest_wider(result) |> 
  mutate(abs_difference = abs(difference)) |> 
  group_by(country_pair) |> 
  mutate(total_diff_country_pair = sum(abs_difference)) |> 
  distinct() |> 
  ungroup()

country_pair_diff_final <- country_pair_diff |> 
  distinct(country_pair,
           total_diff_country_pair)

# What's the max?

country_pair_diff_final |> 
  filter(total_diff_country_pair == max(total_diff_country_pair))


# Graph it again!

final_data |> 
  filter(country_name %in% c("Democratic Republic of the Congo",
                             "Germany")) |> 
  distinct(country_name, category, .keep_all = TRUE) |> 
  mutate(category = category |> 
           as.factor() |> 
           fct_reorder(category_hours_per_day)) |> 
  ggplot() +
  geom_col(aes(x = category,
               y = category_hours_per_day,
               fill = country_name),
           position = "dodge2") +
  labs(x = "Category",
       y = "Hours per day",
       fill = "Country name") +
  coord_flip()

pivot_wider(names_from = station, values_from = seen)

matrix_thingy <- final_data |> 
  distinct(country_name,
           category,
           category_hours_per_day)
