# Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)


# Read raw data -----------------------------------------------------------
cakes <- read_csv(here("raw_data/cake_ingredients_1961.csv"))
ing_codes <- read_csv(here("raw_data/cake_ingredient_code.csv"))


# Pivot cakes df ----------------------------------------------------------
cakes <- cakes %>% 
  pivot_longer(cols = c(AE:ZH),
               names_to = "code",
               values_to = "amount")


# Join dfs ----------------------------------------------------------------
cakes <-cakes %>% 
  inner_join(ing_codes, by = "code")

rm(ing_codes)


# Finish tidying up columns -----------------------------------------------
cakes <- cakes %>% 
  select(Cake, ingredient, measure, amount) %>% 
  rename(
    "cake_name" = "Cake",
    "measure_unit" = "measure")


# Sort amounts ------------------------------------------------------------
cakes <- cakes %>% 
  mutate(
    measure_unit = if_else(
      str_detect(ingredient, "Sour cream cup"), "cup", measure_unit),
    ingredient = if_else(
      str_detect(ingredient, "Sour cream cup"), "Sour cream", ingredient))


# Remove rows of empty ingredients ----------------------------------------
cakes <-drop_na(cakes)



# Write to csv ------------------------------------------------------------
write_csv(cakes, "clean_data/clean_cakes.csv")
rm(cakes)