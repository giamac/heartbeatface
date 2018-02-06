#### Function to read JsPsych-CSV file for HeartBeat Face Experiment

library(tidyverse)
library(jsonlite)
library(magrittr)

readFiles <- function(inputFile) {
  
  # Read Input File
  
  file <- read_csv(inputFile)
  
  
  experimental_data <- file %>%
    filter(trial_type == 'pic-responses') %>%
    select(c(rt,
             stimulus,
             key_press))
  
  # Recode Variables in Experiment to get the stimulus presented on the left and the gender
  
  experimental <- experimental_data %>%
    mutate(gender_image = substr(stimulus,24,24),
           left_image = substr(stimulus,37,37)) %>%
    mutate(peak = case_when(left_image == 'p' ~ 'left',
                            left_image == 'l' ~ 'right')) %>%
    select(c(rt,key_press,peak,gender_image))
  
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
  
  # Get the age
  
  age <- file %>%
    filter(trial_type == 'age') %$%
    fromJSON(responses)$Q0
  
  # Get the gender
  
  gender <- file %>% 
    filter(trial_type == 'gender') %$%
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
                           iri_questions,
                           comments
                           )
  return(experiment)
}