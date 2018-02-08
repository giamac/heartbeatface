#### Function to read JsPsych-CSV file for HeartBeat Face Experiment

library(tidyverse)
library(jsonlite)
library(magrittr)

readFiles <- function(inputFile) {
  
  # Read Input File
  
  file <- read_csv(inputFile)
  
  
  experimental_data <- file %>%
    filter(trial_type == 'pic-responses') %>%
    mutate(response = case_when(key_press == 74 ~ 'right',
                                key_press == 78 ~ 'left')) %>%
    select(c(rt,
             stimulus,
             response))
  
  # Recode Variables in Experiment to get the stimulus presented on the left and the gender
  
  experimental <- experimental_data %>%
    mutate(gender_image = substr(stimulus,24,24),
           stimulus_number = substr(stimulus,24,27),
           left_image = substr(stimulus,37,37)) %>%
    mutate(peak = case_when(left_image == 'p' ~ 'left',
                            left_image == 'l' ~ 'right')) %>%
    select(c(rt,
             response,
             peak,
             stimulus_number, 
             gender_image))
  
  # Get the ID
  
  id <- file %>%
    filter(trial_type == 'subject-id') %>%
    select(responses) %$%
    fromJSON(responses)$Q0
  
  # Get the Questionnaire-Files
  
  iri_data <- file %>%
    filter(trial_type == 'iri-responses') %>%
    select(responses) %$%
    data.frame(fromJSON(responses))
  
  # pics check
  
  pics_check <- file %>%
    filter(trial_type == 'pic-checks') %>%
    select(responses) %$%
    data.frame(fromJSON(responses)) %>%
    rename(picCheck1 = Q0,
           picCheck2 = Q1)
  
  # Get the age
  
  age <- file %>%
    filter(trial_type == 'age') %$%
    fromJSON(responses)$Q0
  
  # Get the gender
  
  gender <- file %>% 
    filter(trial_type == 'gender') %$%
    fromJSON(responses)$Q0
  
  # Get the Difficult
  
  difficulty <- file %>% 
    filter(trial_type == 'difficulty') %$%
    fromJSON(responses)$Q0
  
  # Get the strategy
  
  strategy <- file %>% 
    filter(trial_type == 'strategy') %$%
    fromJSON(responses)$Q0
  
  # Get the recognition
  
  recognition <- file %>% 
    filter(trial_type == 'recognition') %$%
    fromJSON(responses)$Q0
  
  # Get the comments
  
  comments <- file %>% 
    filter(trial_type == 'comments') %$%
    fromJSON(responses)$Q0
  

  
  # merge all data into a data frame
  
  experiment <- data.frame(id,
                           age,
                           gender,
                           experimental,
                           iri_data,
                           difficulty,
                           strategy,
                           recognition,
                           pics_check,
                           comments
                           ) %>%
    mutate(congruence = case_when(response == peak ~ 1,
                                  response != peak ~ 0))
    
  return(experiment)
}