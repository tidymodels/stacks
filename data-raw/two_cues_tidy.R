# this script imports and cleans raw data from Julie Jung's work on
# red-eyed tree frog development for use in examples throughout stacks

# load packages
library(tidyverse)
library(janitor)
library(usethis)

# read in raw data
two_cues_raw <- read_csv("data-raw/two_cues_raw.csv")

# set randomness (for reordering)
set.seed(2)

# some cleaning up
two_cues <-
  two_cues_raw |>
  # rename all variables to snake case!
  dplyr::rename_all(janitor::make_clean_names) |>
  # select off some variable, switch up encodings a bit
  dplyr::transmute(
    stimulus = tolower(stimulus),
    age = age_block_hours,
    time_of_day = case_when(
      time_block_hours <= 5 | time_block_hours >= 20 ~ "night",
      time_block_hours > 5 & time_block_hours < 12 ~ "morning",
      time_block_hours >= 12 & time_block_hours < 20 ~ "afternoon",
      TRUE ~ NA_character_
    ),
    hatched = case_when(
      response == "Hatched" ~ TRUE,
      response == "Not Hatched" ~ FALSE,
      TRUE ~ NA
    ),
    latency = latency_to_hatch_minutes_from_start_of_stimulation,
    length = tadpole_length
  ) |>
  # so that we have at least two numeric columns with complete data
  dplyr::filter(!is.na(length)) |>
  dplyr::slice_sample(n = nrow(.))

# usethis::use_data(two_cues, internal = FALSE, overwrite = TRUE)
