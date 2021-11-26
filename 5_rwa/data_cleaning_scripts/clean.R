# Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)


# Read raw data -----------------------------------------------------------
rwa <- read_csv(here("raw_data/rwa.csv"))


# Select columns relevant to analysis -------------------------------------
rwa <- rwa %>%
  select(Q3:Q22, gender, hand, urban, familysize, education, testelapse, age)



# Reverse some scores, calculate rwa_score -----------
cols_to_reverse <- c("Q4", "Q6", "Q8", "Q9", "Q11", "Q13",
                     "Q15", "Q18", "Q20", "Q21")

rwa <- rwa %>% 
  mutate(across(contains(cols_to_reverse), ~ (9 - .x)),
         rwa_score = rowMeans(rwa[ ,1:20])) %>% 
  select(gender:rwa_score)

rm(cols_to_reverse)



# Rename some values ------------------------------------------------------
rwa <- rwa %>%
  mutate(
    gender = as.factor(case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      gender == 3 ~ "Other")),
    
    urban = as.factor(case_when(
      urban == 1 ~ "Rural",
      urban == 2 ~ "Suburban",
      urban == 3 ~ "Urban")),
    
    hand = as.factor(case_when(
      hand == 1 ~ "Right",
      hand == 2 ~ "Left",
      hand == 3 ~ "Both")),
    
    education = as.factor(case_when(
      education == 1 ~ "Below Secondary",
      education == 2 ~ "Secondary",
      education == 3 ~ "University",
      education == 4 ~ "Postgraduate")),

    age = as.factor(case_when(
      age < 18 ~ "Under 18",
      age <= 25 ~ "18 to 25",
      age <= 40 ~ "26 to 40",
      age <= 60 ~ "41 to 60",
      age > 60 ~ "Over 60"))
    )


# Rename some columns -----------------------------------------------------
rwa <- rwa %>% 
  rename(
    "upbringing" = "urban",
    "age_bracket" = "age",
    "test_time" = "testelapse"
         )


write_csv(rwa, "clean_data/rwa.csv")
rm(rwa)