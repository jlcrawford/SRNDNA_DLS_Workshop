#############################################################################
# Check your participants                                                   #
#############################################################################

# Install and load packages we need
#install.packages("psych")
library(tidyverse)
library(psych)

#Check response times 

esm_clean <- esm_clean %>%
  mutate(
    time_int_start = as.numeric(started_ts - scheduled_ts, units = "secs"),
    time_int_fill  = as.numeric(completed_ts - started_ts, units = "secs")
  )

# Plot sample response time
esm_clean %>% 
  ggplot(aes(x=time_int_fill)) +
  geom_histogram()

# Plot individual participants response time
esm_clean %>% 
  filter(PID==sample(unique(PID),size=50,replace=FALSE))%>% # just looking at a 
  # few random participants 
  ggplot(aes(y=time_int_fill,x=factor(PID))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  coord_flip()

describe(esm_clean$time_int_fill) # ~2 min on average

# Can check whether response time depends on age

esm_clean %>%
  group_by(PID) %>%
  summarise(
    time_int_fill_m = mean(time_int_fill, na.rm = TRUE),
    age = mean(age, na.rm = TRUE)
  ) %>%
  summarise(correlation = cor(time_int_fill_m, age, use = "complete.obs"))

# Flag individual prompt times that might be too short or too long

esm_clean <- esm_clean %>%
  mutate(
    flag_prompt = if_else(time_int_fill < 60 | time_int_fill > 360, TRUE, FALSE)
  )
table(esm_clean$flag_prompt)

# Flag participants whose average response time is too short
participant_flags <- esm_clean %>%
  group_by(PID) %>%
  summarise(time_int_fill_m  = mean(time_int_fill, na.rm = TRUE)) %>%
  mutate(flag_participant = time_int_fill_m  < 60) %>%
  select(PID, flag_participant)

table(participant_flags$flag_participant)

# Join back to esm_clean
esm_clean <- esm_clean %>%
  left_join(participant_flags, by = "PID")

# Check compliance
esm_clean %>% 
  group_by(PID) %>% 
  summarise(
    n_surveys = sum(!is.na(completed_ts)),
    compliance = (sum(!is.na(completed_ts), na.rm=TRUE) / n())
    )%>%
summarise(mean = mean(compliance, na.rm=T),
          sd = sd(compliance,na.rm=T),
          min = min(compliance,na.rm=T),
          max = max(compliance,na.rm=T)
          )

# Does compliance change with age?
esm_clean %>% 
  group_by(PID) %>% 
  summarise(
    n_surveys = sum(!is.na(completed_ts)),
    compliance = sum(!is.na(completed_ts), na.rm=TRUE) / n(),
    age = mean(age, na.rm = TRUE))%>%
    summarise(correlation = cor(compliance, age, use = "complete.obs"))

# Exclude based on compliance (CAUTION)
comp_df <- esm_clean %>%
  group_by(PID) %>%
  summarise(
    n_surveys = sum(!is.na(completed_ts)),
    compliance = sum(!is.na(completed_ts)) / n()
  ) %>%
  filter(compliance > 0.5) %>% # only including anyone who completed more than 50%
  pull(PID)  # get the PIDs that pass check 

#Keep only rows for those participants
esm_clean <- esm_clean %>%
  filter(PID %in% comp_df)

# Alternatively (or in addition): How much missing data for particular variables?
esm_clean %>%
  group_by(PID) %>%
  summarise(
    total_rows = n(),
    rows_missing = sum(is.na(happy)),
    prop_missing = mean(is.na(happy))
  )%>%
  arrange(desc(prop_missing))

# Plot missing data per participant
esm_clean %>%
  group_by(PID) %>%
  summarise(
    total_rows = n(),
    rows_missing = sum(is.na(happy)),
    prop_missing = mean(is.na(happy))
  ) %>%
  ggplot(aes(x = factor(PID), y = prop_missing)) +
  geom_col() +
  labs(y = "Proportion of missing 'happy'") +
  theme_minimal()

# Potential additional steps: remove anyone who has more than 30% of happy ratings missing