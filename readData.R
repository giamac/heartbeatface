# Fri Feb  9 23:54:47 2018 ------------------------------

### Load all pariticipants' data

# Load the function to read the individual files

source('readFiles.R')

library(lme4)
library(brms)

# Make a list of all the files

list_of_data <- list.files(path = 'data', full.names = TRUE)

# Read all the files in the data directory

all_files <- lapply(list_of_data,readFiles)

# Merge them into a big dataFrame

final_df <- do.call('rbind', all_files) %>%
  mutate(response_coded = case_when(response == 'left' ~ 1,
                                    response == 'right' ~ 0))

# Some descritpivies 

numberParticipants <- length(unique(final_df$id))

# Select only the participants with correct experimental procedure (Pictures shown)

final_df_clean <- final_df %>%
  filter(picCheck1 == 'Ja' & picCheck2 == 'Ja')

numberParticipants_clean <- length(unique(final_df_clean$id))

# Mean Age of participants of clean data

age_clean <- final_df_clean %>%
  group_by(id) %>%
  summarise(age = mean(as.numeric(paste(age)))) %>%
  summarise(meanAge = mean(age),
            sdAge = sd(age))

# Statistics 

m1 <- glmer(congruence ~ 1 + (1 | id), data = final_df_clean, family = binomial)
mTendency <- glmer(response_coded ~ 1 + (1|id), data = final_df, family = binomial)


m2 <- glmer(response_coded ~ 1 + peak + (1 + peak| id), data = final_df, family = binomial)

m3 <- brm(formula = congruence ~ 1 + (1 | id), 
          data = final_df_clean,
          family = bernoulli(),
          cores = 4)

m3brm <- brm(formula = response_coded ~ 1 + peak + (1 + peak  | id), 
          data = final_df_clean,
          family = bernoulli())

m3Tend <- brm(formula = response_coded ~ 1 + (1 | id), 
              data = final_df,
              family = bernoulli())

factor(final_df$response)
