# This page is to extract the main measures of the Bilingual CDI as needed

# Clear previous data
rm(list = ls())

#load
library(dplyr)

#Load data of the skills csv's seperatly 
merged_df <- read.csv(file.path('raw-csv', 'Summary', '0.merged_CDI_raw.csv'),
                     header=TRUE,
                     stringsAsFactors=FALSE)

##-------------------------------------------------------------------------#
# Calculating participants # 
# only using completed, filter out all uncompleted 
# Filter out rows where 'completed' is 0
filtered_df <- merged_df[merged_df$completed != 0, ]

# Total participants # ------

# Count total unique participants based on 'ID_lab'
total_participants <- length(unique(filtered_df$ID_lab))
cat("Total participants:", total_participants, "\n")

# Participants by Time Point # --------

# Count participants per time point 'token'
participants_per_token <- aggregate(ID_lab ~ token, data = filtered_df, function(x) length(unique(x)))

# Display the counts
cat("Participants per token:\n")
print(participants_per_token)


## -------------------------------------------------------------------------#
#This provides the extraction of word counts per participant in comprehension and production seperately 
# number of spanish words (es) 
# number of basque words (eu)
# number of total words (es + eu)
# number of concept words (total - doublets)

# Calculate total words for each skill category per participant
words_summary <- filtered_df %>%
  group_by(ID_lab) %>%
  summarise(
    es_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE),
    eu_words_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    es_words_produce = sum(skill_es_produce, na.rm = TRUE),
    eu_words_produce = sum(skill_eu_produce, na.rm = TRUE),
    total_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE) + sum(skill_eu_comprehend, na.rm = TRUE),
    conceptual_words_comprehend = total_words_comprehend - sum(skill_es_comprehend & skill_eu_comprehend, na.rm = TRUE),
    total_words_produce = sum(skill_es_produce, na.rm = TRUE) + sum(skill_eu_produce, na.rm = TRUE),
    conceptual_words_produce = total_words_produce - sum(skill_es_produce & skill_eu_produce, na.rm = TRUE)
  )

# Display the summary
print(words_summary)

# These are the word counts per time point 1,2,3 all separate --------------
# Filter data for each time point and calculate summaries

# Time point 1
words_summary_token_1 <- filtered_df %>%
  filter(token == 1) %>%
  group_by(ID_lab) %>%
  summarise(
    es_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE),
    eu_words_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    es_words_produce = sum(skill_es_produce, na.rm = TRUE),
    eu_words_produce = sum(skill_eu_produce, na.rm = TRUE),
    total_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE) + sum(skill_eu_comprehend, na.rm = TRUE),
    conceptual_words_comprehend = total_words_comprehend - sum(skill_es_comprehend & skill_eu_comprehend, na.rm = TRUE),
    total_words_produce = sum(skill_es_produce, na.rm = TRUE) + sum(skill_eu_produce, na.rm = TRUE),
    conceptual_words_produce = total_words_produce - sum(skill_es_produce & skill_eu_produce, na.rm = TRUE)
  )

# Time point 2
words_summary_token_2 <- filtered_df %>%
  filter(token == 2) %>%
  group_by(ID_lab) %>%
  summarise(
    es_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE),
    eu_words_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    es_words_produce = sum(skill_es_produce, na.rm = TRUE),
    eu_words_produce = sum(skill_eu_produce, na.rm = TRUE),
    total_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE) + sum(skill_eu_comprehend, na.rm = TRUE),
    conceptual_words_comprehend = total_words_comprehend - sum(skill_es_comprehend & skill_eu_comprehend, na.rm = TRUE),
    total_words_produce = sum(skill_es_produce, na.rm = TRUE) + sum(skill_eu_produce, na.rm = TRUE),
    conceptual_words_produce = total_words_produce - sum(skill_es_produce & skill_eu_produce, na.rm = TRUE)
  )

# Time point 3
words_summary_token_3 <- filtered_df %>%
  filter(token == 3) %>%
  group_by(ID_lab) %>%
  summarise(
    es_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE),
    eu_words_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    es_words_produce = sum(skill_es_produce, na.rm = TRUE),
    eu_words_produce = sum(skill_eu_produce, na.rm = TRUE),
    total_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE) + sum(skill_eu_comprehend, na.rm = TRUE),
    conceptual_words_comprehend = total_words_comprehend - sum(skill_es_comprehend & skill_eu_comprehend, na.rm = TRUE),
    total_words_produce = sum(skill_es_produce, na.rm = TRUE) + sum(skill_eu_produce, na.rm = TRUE),
    conceptual_words_produce = total_words_produce - sum(skill_es_produce & skill_eu_produce, na.rm = TRUE)
  )

# Display summaries
print("Words summary for token 1:")
print(words_summary_token_1)

print("Words summary for token 2:")
print(words_summary_token_2)

print("Words summary for token 3:")
print(words_summary_token_3)


# The word counts per time point all in one data frame-------

# Calculate words summary per participant and per time point
words_summary_per_time <- filtered_df %>%
  group_by(ID_lab, token) %>%
  summarise(
    es_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE),
    eu_words_comprehend = sum(skill_eu_comprehend, na.rm = TRUE),
    es_words_produce = sum(skill_es_produce, na.rm = TRUE),
    eu_words_produce = sum(skill_eu_produce, na.rm = TRUE),
    total_words_comprehend = sum(skill_es_comprehend, na.rm = TRUE) + sum(skill_eu_comprehend, na.rm = TRUE),
    conceptual_words_comprehend = total_words_comprehend - sum(skill_es_comprehend & skill_eu_comprehend, na.rm = TRUE),
    total_words_produce = sum(skill_es_produce, na.rm = TRUE) + sum(skill_eu_produce, na.rm = TRUE),
    conceptual_words_produce = total_words_produce - sum(skill_es_produce & skill_eu_produce, na.rm = TRUE)
  )

# Display the updated summary
print(words_summary_per_time)



