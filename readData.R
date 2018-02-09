### Load all pariticipants' data

# Load the function to read the individual files

source('readFiles.R')

library(lme4)

# Make a list of all the files

list_of_data <- list.files(path = 'data/', full.names = TRUE)

# Read all the files in the data directory

all_files <- lapply(list_of_data,readFiles)

# Merge them into a big dataFrame

final_df <- do.call('rbind', all_files) %>%
  mutate(response_coded = case_when(response == 'left' ~ 1,
                                    response == 'right' ~ 0))



m1 <- glmer(congruence ~ 1 + (1 | id), data = final_df, family = binomial)
mTendency <- glmer(response_coded ~ 1 + (1|id), data = final_df, family = binomial)



m2 <- glmer(response_coded ~ 1 + peak + (1 + peak| id), data = final_df, family = binomial)
m3 <- brm(formula = congruence ~ 1, 
          data = final_df,
          family = bernoulli())

m3brm <- brm(formula = response_coded ~ 1 + peak, 
          data = final_df,
          family = bernoulli())
m3Tend <- brm(formula = response_coded ~ 1, 
              data = final_df,
              family = bernoulli())

factor(final_df$response)
