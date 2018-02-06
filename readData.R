### Load all pariticipants' data

# Load the function to read the individual files

source('readFiles.R')

# Make a list of all the files

list_of_data <- list.files(path = 'data/', full.names = TRUE)

# Read all the files in the data directory

all_files <- lapply(list_of_data,readFiles)

# Merge them into a big dataFrame

final_df <- do.call('rbind', all_files)

# Do stuff with the data
write_csv(final_df,'first_files.csv')
