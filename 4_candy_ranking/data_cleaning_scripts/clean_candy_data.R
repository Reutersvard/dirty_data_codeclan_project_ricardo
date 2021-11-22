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

# Also removed sneaky political question in the form of M&M colour.


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
    "despair_other" = 
      "please_list_any_items_not_included_above_that_give_you_despair",
    "other_comments" = 
      "please_leave_any_remarks_or_comments_regarding_your_choices"
  )


# Converting id to character ------------------------------------------------
# This could be numeric type, but char type will suffice for the purposes.
candy_2015 <- candy_2015 %>%
  mutate(id = as.character(id))

candy_2016 <- candy_2016 %>%
  mutate(id = as.character(id))


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
  
  return("No more unmatched col names")
}

# Doesn't quite print the names from df2, but it does the trick.

# Amend the amount of columns found with function  ---------------------------

# For 2017
candy_2017 <- candy_2017 %>% 
  mutate(mary_janes = nrow(.) * NA)

# For 2016
candy_2016 <- candy_2016 %>% 
  mutate(
    sandwich_sized_bags_filled_with_boo_berry_crunch = nrow(.) * NA,
    take_5 = nrow(.) * NA)

# For 2015
candy_2015 <- candy_2015 %>% 
  mutate(
    gender = nrow(.) * NA,
    country = nrow(.) * NA,
    region = nrow(.) * NA,
    take_5 = nrow(.) * NA,
    boxo_raisins = nrow(.) * NA,
    chardonnay = nrow(.) * NA,
    coffee_crisp = nrow(.) * NA,
    dove_bars = nrow(.) * NA,
    hersheys_dark_chocolate = nrow(.) * NA,
    hersheys_kisses = nrow(.) * NA,
    mike_and_ike = nrow(.) * NA,
    mr_goodbar = nrow(.) * NA,
    peeps = nrow(.) * NA,
    reeses_pieces = nrow(.) * NA,
    sourpatch_kids_i_e_abominations_of_nature = nrow(.) * NA,
    sweet_tarts = nrow(.) * NA,
    sweetums_a_friend_to_diabetes = nrow(.) * NA,
    whatchamacallit_bars = nrow(.) * NA,
    tic_tacs = nrow(.) * NA,
    sandwich_sized_bags_filled_with_boo_berry_crunch = nrow(.) * NA
    )


# Combining dfs ------------------------------------------------------

# Combine 2016 and 2017, amend columns so they match 2015
candies <- rbind(candy_2016, candy_2017)

candies <- candies %>% 
  mutate(
    bubble_gum = nrow(.) * NA,
    peanut_butter_bars = nrow(.) * NA,
    peanut_butter_jars = nrow(.) * NA,
    box_o_raisins = nrow(.) * NA,
    brach_products_not_including_candy_corn = nrow(.) * NA,
    dark_chocolate_hershey = nrow(.) * NA,
    hershey_s_kissables = nrow(.) * NA,
    lapel_pins = nrow(.) * NA,
    runts = nrow(.) * NA,
    mint_leaves = nrow(.) * NA,
    mint_m_ms = nrow(.) * NA,
    ribbon_candy = nrow(.) * NA,
    sweetums = nrow(.) * NA,
    peterson_brand_sidewalk_chalk = nrow(.) * NA,
    sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year = 
      nrow(.) * NA
    )

# Combine the two remaining dfs
candies <- candies %>%
  rbind(candy_2015)

# Remove redundant objects
rm(candy_2015, candy_2016, candy_2017, print_unmatched_colnames)


# Recover age values from country column ----------------------------------

relevant_ids <- filter(candies, is.na(age)) %>% 
  arrange(country) %>% 
  head(11) %>% 
  select(id) %>% 
  flatten_chr()
  
candies <- candies %>%
  mutate(age = if_else(id %in% relevant_ids, country, age))

# We can also recover the country of these people given their region.
candies <- candies %>%
  mutate(country = if_else(id %in% relevant_ids, "USA", country))

# Also spotted that id 90288589 has no info
candies <- candies %>% 
  filter(id != "90288589")

rm(relevant_ids)


# Pivot the two remaining dfs ---------------------------------------------
# candies <- candies %>%
#   pivot_longer(cols = c("100_grand_bar":york_peppermint_patties,
#                       sandwich_sized_bags_filled_with_boo_berry_crunch, take_5),
#                names_to = "candy_name",
#                values_to = "rating")
# 
# candy_2015 <- candy_2015 %>%
#   pivot_longer(cols = c(butterfinger:york_peppermint_patties,
#                         necco_wafers,
#                         boxo_raisins:sandwich_sized_bags_filled_with_boo_berry_crunch,
# sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year
#               ),
#                names_to = "candy_name",
#                values_to = "rating")
#   





