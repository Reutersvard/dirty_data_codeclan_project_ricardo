# Load libraries ----------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)

# Read in raw data --------------------------------------------------------
candy_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx"))
candy_2016 <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx"))
candy_2017 <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx"))



# clean_names and remove columns ------------------------------------------

# Cleaning column names and removing unimportant columns from 2015 data
candy_2015 <- candy_2015 %>% 
  janitor::clean_names() %>% 
  select(-contains(c("please_estimate",
                   "which_day",
                   "taylor_swift",
                   "that_dress",
                   "betty_or",
                   "check_all_that_apply_i_cried",
                   "guess_the_number",
                   "fill_in_the",
                   "favourite_font",
                   "if_you_squint")))

# Cleaning column names and removing unimportant columns from 2016 data
candy_2016 <- candy_2016 %>% 
  janitor::clean_names() %>% 
  select(-contains(c("please_estimate_",
                     "which_day",
                     "that_dress",
                     "betty_or",
                     "do_you_eat_apples",
                     "guess_the_number",
                     "blue_m",
                     "red_m",
                     "third_party_m",
                     "when_you_see_the_above_image",
                     "favourite_font",
                     "york_peppermint_patties_ignore",
                     "person_of_interest_season_3",
                     "bonkers_the_board_game")))

# Cleaning column names and removing unimportant columns from 2017 data
candy_2017 <- candy_2017 %>% 
  janitor::clean_names() %>% 
  select(-contains(c("click_coordinates",
                     "q12",
                     "q11",
                     "x114",
                     "q10",
                     "do_you_eat_apples",
                     "guess_the_number",
                     "when_you_see_the_above_image",
                     "favourite_font",
                     "blue_ray",
                     "bonkers_the_board_game",
                     "blue_m",
                     "red_m",
                     "green_party_m",
                     "abstained_from_m",
                     "independent_m")))
# Removed sneaky political question in the form of M&M colour.


# Rename columns ----------------------------------------------------------

# Renaming columns for candy_2017
names(candy_2017) <- names(candy_2017) %>%
  str_sub(4)

candy_2017 <- candy_2017 %>% 
  rename(
    "id" = "ernal_id",
    "region" = "state_province_county_etc",
    "bonkers" = "bonkers_the_candy",
    "licorice" ="licorice_yes_black",
    "anonymous_brown_globs_that_come_in_black_and_orange_wrappers" = 
 "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes"
    )

names(candy_2016)

# Renaming for 2016
candy_2016 <- candy_2016 %>% 
  rename(
    "id" = "timestamp",
    "going_out" = "are_you_going_actually_going_trick_or_treating_yourself",
    "gender" = "your_gender",
    "age" = "how_old_are_you",
    "country" = "which_country_do_you_live_in",
    "region" = "which_state_province_county_do_you_live_in",
    "100_grand_bar" = "x100_grand_bar",
    "bonkers" = "bonkers_the_candy",
    "licorice" ="licorice_yes_black",
    "joy_other" = "please_list_any_items_not_included_above_that_give_you_joy",
    "despair_other" = 
      "please_list_any_items_not_included_above_that_give_you_despair",
    "other_comments" =
"please_leave_any_witty_snarky_or_thoughtful_remarks_or_comments_regarding_your_choices"
         )

# Renaming for 2015
candy_2015 <- candy_2015 %>% 
  rename(
    "id" = "timestamp",
    "going_out" = "are_you_going_actually_going_trick_or_treating_yourself",
    "age" = "how_old_are_you",
    "100_grand_bar" = "x100_grand_bar",
    "joy_other" = "please_list_any_items_not_included_above_that_give_you_joy",
    "despair_other" = "please_list_any_items_not_included_above_that_give_you_despair",
    "other_comments" = "please_leave_any_remarks_or_comments_regarding_your_choices"
  )


# Helper function to find remaining unmatched column names ----------------
print_unmatched_colnames <- function(df1, df2) {
  
  # Get the column names of the two dfs and the max number, initialise answer
  names1 <- names(df1)
  names2 <- names(df2)
  len <- 1:max(length(names1), length(names2))
  
  # Loop over both lists of names and print unmatched ones
  for (i in len) {
    if (names1[i] %in% names2) {
      next
    } else {
        print(paste(
          "Col no.", i, names1[i], "has no match in", substitute(df2)))
      }
    
      if (names2[i] %in% names1) {
        next
      } else {
        print(paste(
          "Col no.", i, names2[i], "has no match in", substitute(df1)))
      }
  }
  
  return("No more unmatched names")
}

